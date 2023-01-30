(ns booru-meta.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [typed.clojure :as t]
            [clj-http.client :as client]
            [progrock.core :as pr]
            [cheshire.core :as json])
  (:import (java.nio.file FileSystems)))

;; https://github.com/babashka/fs
;; https://stackoverflow.com/questions/5019978/what-is-the-most-concise-clojure-equivalent-for-rubys-dir-glob
(def exts ["jpg" "jpeg" "png" "webp" "gif" "webm"])

;; okay you have to use two stars (idk why)
(defn glob [path pattern]
  (let [files (file-seq (io/file path))
        java-fs (java.nio.file.FileSystems/getDefault)
        matcher (.getPathMatcher java-fs (str "glob:" pattern))]
    (filter (fn [file]
              (let [path (.toPath file)]
                (.matches matcher path)))
            files)))
(t/ann glob [t/Str t/Str :-> (t/Seq java.io.File)])

(defn make-pattern [exts]
  (str "**.{"
       (string/join "," exts)
       "}"))

(defmulti md5? type)
(defmethod md5? :default [_x] false)
(defmethod md5? String [s]
  (if (not= (count s) 32)
    false
    (some? (re-find #"^[0-9a-f]+$" s))))
(defmethod md5? java.io.File [file]
  (let [stem (com.google.common.io.Files/getNameWithoutExtension (str file))
        stem (string/lower-case stem)]
    (md5? stem)))


(type "123")
(md5? "ac12bf37522848b27ec31b532d313ea1")

(defn cats-md5
  [files]
  (let [files-with-type (map (fn [path] {:file path :type (if (md5? path) :md5 :normal)}) files)]
    (reduce (fn [acc {:keys [file type]}]
              (assoc acc type (conj (get acc type) file)))
            {}
            files-with-type)))

(def files (glob "/Volumes/Untitled 1/Grabber" (make-pattern exts)))
(cats-md5 files)

(def user-agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.71 Safari/537.36")

;; sync
;; https://github.com/dakrone/clj-http#basic-auth
;; https://danbooru.donmai.us/wiki_pages/help:api
;; %3A = ":"
;; site option
;; {:name :danbooru
;;  :url "https://danbooru.donmai.us/posts.json"
;;  :make-param (fn [md5] {:tags (str "md5:" md5)})
;;  :preprocess (fn [content] (if (seq? content) nil (first content)))}
(defn get-metadata [options]
  (let [url (:url options)
        header {"User-Agent" user-agent}
        preprocess (:preprocess options)
        param (:param options)
        source (:name options)]
    (let [content (:body (client/get url {:headers header :content-type :json :as :json :query-params param}))
          processed (preprocess content)]
      (if (some? processed) {:data processed :source source} nil))))

(defn get-metadata-danbooru [md5]
  (get-metadata {:name :danbooru
                 :url "https://danbooru.donmai.us/posts.json"
                 :param {:tags (str "md5:" md5)}
                 :preprocess (fn [content] (if (seq? content) nil (first content)))}))

;; it's only return a json and next and prev
;; https://www.npmjs.com/package/sankaku-api?activeTab=explore
;; https://capi-v2.sankakucomplex.com/posts/keyset
(defn get-metadata-sankaku [md5]
  (get-metadata {:name :sankaku
                 :url "https://capi-v2.sankakucomplex.com/posts/keyset"
                 :param {:tags (str "md5:" md5)}
                 :preprocess #(first (:data %))}))

;; why the tags is placed at outside?
(defn get-metadata-yandere [md5]
  (get-metadata {:name :yandere
                 :url "https://yande.re/post.json"
                 :param {:api_version 2 :tags (str "md5:" md5) :include_tags 1}
                 :preprocess (fn [content] (let [post (first (:posts content))
                                                 tags (:tags content)]
                                             (if (some? post) (assoc post :tags tags) nil)))}))


(get-metadata-yandere "20aeb65cd6241b2007f415bfe09afb2e")
(get-metadata-sankaku "f6f3fc979c1609372e491d101ba51f09")
(get-metadata-danbooru "f6f3fc979c1609372e491d101ba51f09")
