(ns booru-meta.common
  (:require [malli.core :as m]
            [malli.util :as mu])
  (:use [clojure.core.async :only [chan >!! close!]])
  (:import [java.util.concurrent Executors Executor]
           [clojure.lang Var]))

(def default-user-agent
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.71 Safari/537.36")


;; It's rediculous that `thread` macro would not limit the number of threads
;; https://blog.csdn.net/denghonghao/article/details/82428531
;; https://www.baeldung.com/java-executors-cached-fixed-threadpool
;;  **reasonable** number of short-lived tasks?
(defonce ^:private ^Executor compress-executor (Executors/newFixedThreadPool 6))

(defn image-thread-call
  "Copy and pasted from `clojure.core.async/thread-call`"
  [f]
  (let [c (chan 1)]
    (let [binds (Var/getThreadBindingFrame)]
      (.execute compress-executor
                (fn []
                  (Var/resetThreadBindingFrame binds)
                  (try
                    (let [ret (f)]
                      (when-not (nil? ret)
                        (>!! c ret)))
                    (finally
                      (close! c))))))
    c))

(defmacro image-thread
  "Copy and pasted from `clojure.core.async."
  [& body]
  `(image-thread-call (^:once fn* [] ~@body)))
