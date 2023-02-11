(ns booru-meta.common
  (:require [malli.core :as m]
            [malli.util :as mu]
            [clj-http.conn-mgr :as conn-mgr])
  (:use [clojure.core.async :only [chan >!! close!]])
  (:import [java.util.concurrent Executors Executor]
           [clojure.lang Var]))

(def default-user-agent
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:107.0) Gecko/20100101 Firefox/107.0")


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

;; https://github.com/dakrone/clj-http#proxies
(defonce 
  proxy-options 
  {:proxy-host "127.0.0.1" :proxy-port 36000})
