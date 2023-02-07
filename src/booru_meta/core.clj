(ns booru-meta.core
  (:require [babashka.fs :as fs]
            [booru-meta.booru :as booru]
            [booru-meta.schema :as schema]
            [cheshire.core :as json]
            [clojure.core.async :as a]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.term.colors :as tc]
            [clojure.tools.cli :refer [parse-opts]]
            [malli.core :as m]
            [clojure.tools.logging :as log]
            [progrock.core :as pr]
            [booru-meta.sauce :as sauce]
            [again.core :as again])
  (:use [booru-meta.utils])
  (:gen-class))

;; https://stackoverflow.com/questions/6694530/executing-a-function-with-a-timeout
(defn retry-when-error
  "Retry when error is not nil or keyword which means exception. 
   retry three times `[500 1000 3000]`
   Should run in a go block.
   `on-error` would be called with the error."
  [f & {:keys [on-error] :or {on-error (constantly nil)}}]
  (loop [res (f)
         retry [500 1000 3000]]
    (if-let [error (:error res)]
      (when (and (some? (first retry)) (not (keyword? error)))
        (a/<! (a/timeout (first retry)))
        (on-error error)
        (recur (f) (rest retry)))
      res)))

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
  "a group of functions to query booru/sauce
   return a seq of result"
  [fns & args]
  (loop [[func & rest-fns] fns
         resps []]
    (if (fn? func)
      (let [res (retry-when-error #(deref (apply func args)) :on-error log/error)
            data (:data res)]
        (if (some? data) resps
            (recur rest-fns (conj resps res))))
      resps)))

(defn query-booru [& args]
  (loop-query [booru/danbooru booru/sankaku booru/yandere] args))

(defn query-sauce [& args]
  (loop-query [sauce/sauce sauce/iqdb] args))

(defn categorize-results 
  [resps]
  (if (m/validate (m/schema [:sequential schema/result]) resps)
    (reduce (fn [acc {:keys [data] :as resp}]
              (if (some? data)
                (assoc acc :data (merge (get acc :data) (value-as-key resp :data)))
                (assoc acc :error (merge (get acc :error) (value-as-key resp :source)))))
            {:data {} :error {}} resps)
    {}))

(defn mk-persistent-template [file & {:keys [root-path] :or {root-path nil}}]
  (let [path (mk-path file root-path)]
    {:data {}
     :md5 ((comp bytes->string calc-md5) file)
     :path path
     :version "0.1"}))

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
(defn read-aux-json [file]
  (let [data-path (with-extension file ".json")
        nomatch-path (with-extension file ".nomatch.json")
        read-as-str (fn [file] (s/join "\n" (fs/read-all-lines file)))]
    {:data (if (fs/exists? data-path) (-> data-path read-as-str json/decode) nil)
     :error (if (fs/exists? nomatch-path) (-> nomatch-path read-as-str json/decode) nil)}))

(defn save-aux-json [file {data :data error :error}]
(let [data-path (with-extension file ".json")
      nomatch-path (with-extension file ".nomatch.json")]
  (when (seq data)
    (fs/write-lines data-path [(json/encode data)]))
  (when (seq error)
    (fs/write-lines nomatch-path [(json/encode error)]))
  nil))

(defn query-save [file & {:keys [root-path] :or {root-path nil}}]
  (let [stem (file->stem file)
        results (-> stem query-booru categorize-results)
        {last-data :data last-nomatch :error} (read-aux-json file)
        merged (if (some? last-data)
                 (merge-categorized-results results :last-data last-data :last-nomatch last-nomatch)
                 (merge-categorized-results results 
                                            :data-template (mk-persistent-template file :root-path root-path) 
                                            :last-nomatch last-nomatch))]
    (save-aux-json file merged)))

(defn run-batch
  [file-list handler &
   {:keys [max-limit reset-interval-ms root-path random-delay-ms]
    :or {max-limit 17
         reset-interval-ms 30000
         root-path nil
         random-delay-ms [0 100]}}]
  (let [short-limit (atom 0)
        file-list (filter #(not (fs/exists? (with-extension % "json"))) file-list)
        reset-limit #(reset! % 0)
        cancel (interval reset-limit [short-limit] reset-interval-ms)
        flag (atom true)
        bar (atom (pr/progress-bar (count file-list)))
        bar-chan (a/chan 10)
        action (fn [file]
                 (handler file :root-path root-path)
                 (swap! bar pr/tick)
                 (a/put! bar-chan @bar))]
    (assert (fn? handler) "handler should be a function")
    (doseq [file file-list]
      ;; skip when json file exists
      (a/go-loop []
        (when @flag
          (if (>= @short-limit max-limit)
            (do (a/<! (a/timeout 500)) (recur))
            (do (a/<! (a/timeout (apply rand-int-range random-delay-ms)))
                (swap! short-limit inc)
                (action file))))))
    {:cancel #(do (cancel)
                  (reset! flag false))
     :bar-chan bar-chan}))

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
      (run-batch file-list query-save {:root-path root}))))

