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
        default {:max-limit 17 :reset-interval-ms 30000 :root-path nil}
        options (merge default options)
        reset-limit #(reset! % 0)
        cancel (interval reset-limit [short-limit] (:reset-interval-ms options))
        flag (atom true)
        action (fn [file]
                 (a/go
                   (let [stem (file->stem file)
                         info @(booru/danbooru stem)
                         path (if (some? (:root-path options))
                                (fs/relativize (fs/path (:root-path options)) (fs/path (str file))) (str file))]
                     (if (some? (:data info))
                       (do
                         ((comp #(fs/write-lines (with-extension file "json") [%]) json/encode)
                          (merge info {:md5 stem
                                       :real-md5 ((comp bytes->string calc-md5) file)
                                       :path path}))
                         (println (format "%s" (str path))))
                       (println (format "%s not found" (str path)))))))]
    (doseq [file file-list]
      (when @flag
        (a/go-loop []
          (if (>= @short-limit (:max-limit options))
            (when @flag (a/<! (a/timeout 500)) (recur))
            (do (action file)
                (swap! short-limit inc))))))
    #(do (cancel)
         (reset! flag false))))
