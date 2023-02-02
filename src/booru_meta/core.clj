(ns booru-meta.core
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [typed.clojure :as t]
            [clj-http.client :as client]
            [progrock.core :as pr]
            [cheshire.core :as json]
            [clojure.string :as s]
            [bites.core]
            [booru-meta.sauce :as sauce]
            [booru-meta.booru :as booru]
            [clojure.core.async :as a])
  (:import (java.nio.file FileSystems)
           org.apache.commons.codec.binary.Hex)
  (:use [booru-meta.utils]))

;; https://github.com/clojure/core.async/blob/master/examples/walkthrough.clj
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
                              (str (fs/relativize (fs/path (:root-path options)) (fs/path (str file)))) (str file))]
                   (if (some? (:data info))
                     (do ((comp #(fs/write-lines (with-extension file "json") [%]) json/encode)
                          (merge info {:md5 stem
                                       :real-md5 ((comp bytes->string calc-md5) file)
                                       :path path}))
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
