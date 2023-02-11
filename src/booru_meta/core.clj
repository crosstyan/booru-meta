(ns booru-meta.core
  (:require [babashka.fs :as fs]
            [booru-meta.booru :as booru]
            [booru-meta.schema :as schema]
            [cheshire.core :as json]
            [clojure.core.async :as a]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as cljset]
            [clojure.term.colors :as tc]
            [clojure.tools.cli :refer [parse-opts]]
            [malli.core :as m]
            [clojure.tools.logging :as log]
            [progrock.core :as pr]
            [booru-meta.sauce :as sauce]
            [again.core :as again]
            [clojure.walk :as walk])
  (:use [booru-meta.utils]
        [booru-meta.common]
        [clojure.core.async :only [go go-loop <! <!! >! timeout chan]])
  (:gen-class))

;; https://stackoverflow.com/questions/6694530/executing-a-function-with-a-timeout
(defn retry-when-error
  "Function `f` should return a channel.

   Return a channel.
   
   Retry when error is not nil or keyword which means exception. 
   retry three times `[500 1000 3000]`
   `on-error` would be called with the error."
  [f & {:keys [on-error] :or {on-error (constantly nil)}}]
  (go-loop [chan (f)
            [retry & remain] [500 1000 3000]]
    (let [res (if (chan? chan) (<! chan) chan)]
      (if-let [error (:error res)]
        (if (and (some? retry) (not (keyword? error)))
          (do (<! (timeout retry))
              (on-error {:error error :retry retry :remain remain})
              (recur (f) remain))
          res)
        res))))

;; interface
;; Info returned from booru/saurce should be
;; (Data {:data Hashmap|nil, :source keyword, :error keyword|exception|nil}) 
;; Write to JSON
;; (Path {:absolute string, :relative string|nil})
;; {:data (Seq Data), :md5 string, :path string, :version string (the version of scheme)}

;; https://github.com/clojure/core.async/blob/master/examples/walkthrough.clj
;; TODO: phash?
;; https://github.com/nihas101/pHash
;; https://github.com/KilianB/JImageHash
(defn mk-path ([path] {:absolute (str path)})
  ([path root-path]
   (if (some? root-path)
     {:absolute (str path)
      :relative (str (fs/relativize (fs/path root-path) (fs/path (str path))))}
     (mk-path path))))

(def method-map
  {:danbooru booru/danbooru
   :sankaku booru/sankaku
   :yandere booru/yandere
   :saucenao sauce/sauce
   :iqdb sauce/iqdb
   :ascii2d sauce/ascii2d})

(defn value-as-key
  "make `{:k v}` `->` `{v {:k v}}`
 won't convert v to keyword."
  [m k]
  (if-let [v (get m k)]
    (assoc {} v m)
    {}))

(defn loop-query
  "Return a channel

   a group of functions to query booru/sauce
   return a seq of result"
  [fns & args]
  (go-loop [[func & rest-fns] fns
            resps []]
    (if (fn? func)
      (let [on-error
            (fn [{error :error retry :retry remain :remain}]
              (log/error {:error (:cause (Throwable->map error))
                          :retry retry :remain remain}))
            res (<! (retry-when-error #(apply func args)
                                      :on-error on-error))
            data (:data res)]
        (if (some? data) (conj resps res)
            (recur rest-fns (conj resps res))))
      resps)))

(defn query-booru [args]
  (loop-query (shuffle [booru/danbooru booru/sankaku]) args))


(defn query-sauce [args]
  (loop-query [sauce/iqdb #(sauce/sauce % {:api-key "009934e06a88a3a1f28c565d69a5273ee47008e1"})] args))

(defn categorize-results
  [resps]
  (if (m/validate (m/schema [:sequential schema/result]) resps)
    (reduce (fn [acc {:keys [data] :as resp}]
              (if (some? data)
                (assoc acc :data (merge (get acc :data) (value-as-key resp :source)))
                (assoc acc :error (merge (get acc :error) (value-as-key resp :source)))))
            {:data {} :error {}} resps)
    (do (log/warn "Invalid result" resps)
        {})))

(defn mk-persistent-template [file & {:keys [root-path] :or {root-path nil}}]
  (let [path (mk-path file root-path)]
    {:data {}
     :md5 ((comp bytes->string calc-md5) file)
     :path path
     :version "0.1"}))

(defn async-mk-persistent-template [file & {:keys [root-path] :or {root-path nil}}]
  (go (let [path (mk-path file root-path)
            md5 (<! (a/thread (calc-md5 file)))]
        {:data {}
         :md5 md5
         :path path
         :version "0.1"})))


;; https://stackoverflow.com/questions/2753874/how-to-filter-a-persistent-map-in-clojure
;; https://stackoverflow.com/questions/28408743/how-do-you-destructure-a-map-into-key-value-pairs-without-knowing-the-keys-in-cl
;; https://stackoverflow.com/questions/1676891/mapping-a-function-on-the-values-of-a-map-in-clojure
;; TODO: write tests. 
(defn merge-categorized-results
  "`data` and `error` should be a map whose key is source and value is a map."
  [{data :data error :error} & {:keys [last-data last-nomatch data-template]}]
  (assert (not (and (nil? last-data) (nil? data-template))))
  (let [nomatch (into {} (filter #(= :no-match (:error (val %)))) error)
        nomatch (update-vals nomatch (constantly true))
        new-data (if (some? last-data)
                   (assoc last-data :data (merge (get last-data :data) data))
                   (assoc data-template :data data))]
    {:data new-data :error (merge last-nomatch nomatch)}))

;; https://stackoverflow.com/questions/30205407/translate-encoding-of-string
(defn read-aux-json
  "Side effect: read json file and return a map."
  [file]
  (let [data-path (with-extension file ".json")
        nomatch-path (with-extension file ".nomatch.json")
        ;; json/decode would return string as map key. 
        ;; have to use walk/keywordize-keys to convert string to keyword key.
        data (if (fs/exists? data-path) (-> data-path str slurp json/decode walk/keywordize-keys) nil)
        nomatch (if (fs/exists? nomatch-path) (-> nomatch-path str slurp json/decode walk/keywordize-keys) nil)]
    {:data (if (m/validate schema/persistent-info data) data nil)
     :error (if (m/validate schema/persistent-info nomatch) nomatch nil)}))

(defn save-aux-json
  "Side effect: save data and error to file."
  [file {data :data error :error}]
  (let [data-path (with-extension file ".json")
        nomatch-path (with-extension file ".nomatch.json")]
    (when (seq (:data data))
      (spit (str data-path) (json/encode data)))
    (when (seq error)
      (spit (str nomatch-path)  (json/encode error)))
    nil))

(defn async-save-results [file results & {:keys [root-path] :or {root-path nil}}]
  (go (let [{last-data :data last-nomatch :error} (read-aux-json file)
            merged (if (some? last-data)
                     (merge-categorized-results results :last-data last-data :last-nomatch last-nomatch)
                     (merge-categorized-results results
                                                :data-template (<! (async-mk-persistent-template file :root-path root-path))
                                                :last-nomatch last-nomatch))]
        (<! (a/thread (save-aux-json file merged))))))

(defn query-by-md5-then-save
  "Return a channel that will send result
   
   query booru by md5. If filename is md5 like use that value or calculate md5.
  save result to json file.

   an optional `failed-chan` can be passed to send failed file to it."
  [file & {:keys [root-path force-calc-md5 failed-chan]
           :or {root-path nil force-calc-md5 false}}]
  (go (let [stem (file->stem file)
            query (if force-calc-md5
                    (bytes->string (<! (a/thread (calc-md5 file))))
                    (if (md5? stem) stem
                        (bytes->string (<! (a/thread (calc-md5 file))))))
            results (-> query query-booru <! categorize-results)]
        (when (and (chan? failed-chan) (empty? (:data results)))
          (a/put! failed-chan file))
        (<! (async-save-results file results :root-path root-path))
        results)))

;; TODO: fix no match is not saved.
(defn query-by-file-then-save
  "Return a channel that will send result

   query sauce by file. save result to json file."
  [file & {:keys [root-path failed-chan]
           :or {root-path nil}}]
  (go (let [put-fail (fn [results]
                       (when (and (chan? failed-chan) (empty? (:data results)))
                         (a/put! failed-chan file)))
            compressed (<! (image-thread (read-compress-img file)))
            sauce-results  (-> compressed query-sauce <!  categorize-results)
            {sauce-sucess :data sauce-error :error} sauce-results]
        (if (seq sauce-sucess)
          (let [maybe-fns (map booru/sauce->booru (vals sauce-sucess))
                fns (filter #(fn? (:func %)) maybe-fns)
                chans (map #(let [res (retry-when-error (:func %))] res) fns)
                  ;; how do I use map to get the result from channel?
                rs (go-loop [[c cs] chans
                             resps []]
                     (if (chan? c)
                       (let [resp (<! c)]
                         (recur cs (conj resps resp)))
                       resps))
                {booru-sucess :data booru-error :error}
                (categorize-results (<! rs))
                results {:data (merge sauce-sucess booru-sucess)
                         :error (merge sauce-error booru-error)}]
            (do (<! (async-save-results file results :root-path root-path))
                (put-fail file)
                results))
          (do (<! (async-save-results file sauce-results :root-path root-path))
              (put-fail file)
              sauce-results)))))

(defn query-sauce-2-booru-then-save
  "`func` is the function generated by `sauce->booru`"
  [file func & {:keys [root-path] :or {root-path nil}}]
  (go (let [chan (func)
            booru-result (<! chan)
            booru-result (categorize-results [booru-result])]
        (<! (async-save-results file booru-result :root-path root-path))
        booru-result)))

(defn query-sauce-for-fails
;; https://stackoverflow.com/questions/41066630/how-to-get-the-buffer-size-of-a-channel-from-clojure-core-async
  "query sauce for failed files. Use with `query-by-md5-then-save` and
`run-batch`."
  [failed-chan]
  (let [bar-chan (chan 1024)
        bar_ (atom (pr/progress-bar (.count (.buf failed-chan))))
        file-set_ (atom #{})]
    (go-loop []
      (let [file (<! failed-chan)]
        ;; once at a time, don't hurry.
        (when (some? file)
          (<! (query-by-file-then-save file)))
        (let [buf (.buf failed-chan)
              chan-set (set buf)]
          (swap! file-set_ #(cljset/union % chan-set))
          (swap! bar_ #(assoc % :total (count @file-set_)))
          (swap! bar_ pr/tick)
          (a/put! bar-chan @bar_))
        (recur)))
    {:bar-chan bar-chan}))

(defn run-batch
  "`handler` should return a channel."
  [file-list handler &
   {:keys [max-limit reset-interval-ms root-path random-delay-ms]
    :or {max-limit 17
         reset-interval-ms 30000
         root-path nil
         random-delay-ms [0 100]}}]
  (let [short-limit_ (atom 0)
        reset-limit #(reset! % 0)
        cancel (interval reset-limit [short-limit_] reset-interval-ms)
        flag_ (atom true)
        failed-chan (chan 4096)
        bar_ (atom (pr/progress-bar (count file-list)))
        bar-chan (chan 4096)
        action (fn [file]
                 (go (let [_ (<! (handler file :root-path root-path :failed-chan failed-chan))]
                       (swap! bar_ pr/tick)
                       (a/put! bar-chan @bar_))))]
    (assert (fn? handler) "handler should be a function")
    ;; don't block the main repl thread
    (a/thread
      (doseq [file file-list]
        (loop []
          (when @flag_
            (if (>= @short-limit_ max-limit)
              (do (<!! (a/timeout 500)) (recur))
              (do (<!! (timeout (apply rand-int-range random-delay-ms)))
                  (swap! short-limit_ inc)
                  (action file)))))))
    {:cancel #(do (cancel)
                  (reset! flag_ false)
                  (a/close! failed-chan)
                  (a/close! bar-chan))
     :bar-chan bar-chan
     :failed-chan failed-chan}))

(def args-opts
  [["-i" "--input PATH" "Root path of images"
    :default nil]
   ["-h" "--help"]])

(defn -main [& args]
  (let [parsed (parse-opts args args-opts)
        opts (:options parsed)
        root (:input opts)
        file-list (:md5 (categorize-by-md5 (io/file root)))]
    (if (nil? root)
      (println
       (s/join "\r\n" ["booru-meta"
                       (:summary parsed)
                       (tc/red "error: --input is required")]))
      (run-batch file-list query-by-md5-then-save {:root-path root}))))
