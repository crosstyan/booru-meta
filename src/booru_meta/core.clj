(ns booru-meta.core
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [typed.clojure :as t]
            [clj-http.client :as client]
            [progrock.core :as pr]
            [cheshire.core :as json]
            [clojure.string :as s]
            [clojure.term.colors :as tc]
            [booru-meta.sauce :as sauce]
            [booru-meta.booru :as booru]
            [clojure.core.async :as a]
            [clojure.tools.cli :refer [parse-opts]]
            [booru-meta.schema :as schema])
  (:import (java.nio.file FileSystems)
           org.apache.commons.codec.binary.Hex)
  (:use [booru-meta.utils])
  (:gen-class))

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
(defn run-batch
  [file-list options]
  (let [short-limit (atom 0)
        file-list (filter #(not (fs/exists? (with-extension % "json"))) file-list)
        default {:max-limit 17 :reset-interval-ms 30000 :root-path nil :random-delay-ms [0 100]}
        options (merge default options)
        reset-limit #(reset! % 0)
        cancel (interval reset-limit [short-limit] (:reset-interval-ms options))
        flag (atom true)
        bar (atom (pr/progress-bar (count file-list)))
        action (fn [file]
                 (let [stem (file->stem file)
                       info @(booru/danbooru stem)
                       path (if (some? (:root-path options))
                              {:absolute (str file)
                               :relative (str (fs/relativize (fs/path (:root-path options)) (fs/path (str file))))}
                              {:absolute (str file)})]
                   (if-let [data (:data info)]
                     (do ((comp #(fs/write-lines (with-extension file "json") [%]) json/encode)
                          (merge {:data [data]}
                                 {:md5 ((comp bytes->string calc-md5) file)
                                  :path path
                                  :version "0.1"}))
                         (pr/print @bar))
                     nil)
                   (swap! bar pr/tick)))]
    (doseq [file file-list]
      ;; skip when json file exists
      (a/go-loop []
        (when @flag
          (if (>= @short-limit (:max-limit options))
            (do (a/<! (a/timeout 500)) (recur))
            (do (a/<! (a/timeout (apply rand-int-range (:random-delay-ms options))))
                (swap! short-limit inc)
                (action file))))))
    #(do (cancel)
         (reset! flag false))))

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
      (run-batch file-list {:root-path root}))))

