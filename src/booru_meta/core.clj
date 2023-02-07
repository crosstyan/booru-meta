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
  "Retry when error is not nil or keyword. 
   Should run in a go block.
   `on-error` would be called with the error."
  [f & {:keys [on-error] :or {on-error #(constantly %)}}]
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

(defn query-booru [& args]
  (loop [booru-fns [booru/danbooru booru/sankaku]
         resps []]
    (let [[func & rest-fns] booru-fns]
      (if (fn? func)
        (let [res (retry-when-error #(deref (apply func args)) :on-error log/error)
              data (:data res)]
          (if (some? data) resps
              (recur rest-fns (conj resps res))))
        resps))))

(defn query-save [file & {:keys [root-path] :or {root-path nil}}]
  (let [stem (file->stem file)
        resps (query-booru stem)
        path (mk-path file root-path)]
    ;; (if-let [data (:data info)]
    ;;   (let [d (merge {:data (assoc {} (:source data) data)}
    ;;                  {:md5 ((comp bytes->string calc-md5) file)
    ;;                   :path path
    ;;                   :version "0.1"})]
    ;;     (assert (m/validate schema/persistent-info d) "Invalid schema")
    ;;     ((comp #(fs/write-lines (with-extension file ".json") [%]) json/encode) d))
    ;;   (when (= (:error info) :no-match)
    ;;     ((comp #(fs/write-lines (with-extension file ".nomatch.json") [%]) json/encode)
    ;;      (assoc {} (:source info) true))))
    ))

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

