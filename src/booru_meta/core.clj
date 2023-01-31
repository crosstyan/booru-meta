(ns booru-meta.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [typed.clojure :as t]
            [clj-http.client :as client]
            [progrock.core :as pr]
            [cheshire.core :as json]
            [clojure.string :as s])
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

(defn cats-md5
  [files]
  (let [files-with-type (map (fn [path] {:file path :type (if (md5? path) :md5 :normal)}) files)]
    (reduce (fn [acc {:keys [file type]}]
              (assoc acc type (conj (get acc type) file)))
            {}
            files-with-type)))

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
        source (:name options)
        ret (promise)]
    (do (client/get url {:headers header :content-type :json :as :json :query-params param :async true}
                    (fn [res] (let [processed (preprocess (:body res))
                                    wrapped (if (some? processed) {:data processed :source source} nil)]
                                (deliver ret wrapped)))
                    (fn [_err] (deliver ret nil)))
        ret)))

(defn get-metadata-danbooru [md5]
  (get-metadata {:name :danbooru
                 :url "https://danbooru.donmai.us/posts.json"
                 :param {:tags (str "md5:" md5)}
                 :preprocess (fn [content]
                               (let [original (if (seq? content) nil (first content))
                                     split (fn [s] (if (and (= (type s) String) (not= s "")) (s/split s #"\s+") []))]
                                 {:original original
                                  :final {:id (:id original)
                                          :rating (:rating original)
                                          :md5 (:md5 original)
                                          :general (split (:tag_string_general original))
                                          :artist (split (:tag_string_artist original))
                                          :copyright (split (:tag_string_copytright original))
                                          :character (split (:tag_string_character original))
                                          :meta (split (:tag_string_meta original))}}))}))

;; https://gelbooru.com/index.php?page=wiki&s=view&id=18780
;; Have to use General csv/h5 database to get the metadata of tags. 
;; https://github.com/slpkbt/gelbooru-api/blob/37353af533946062dcb9cb0bacd2c984e5471991/index.js#L62
;; https://github.com/rainyDayDevs/pygelbooru/blob/c0b443aa1fa11e85a43968968ebf244a1bf5c55c/pygelbooru/gelbooru.py#L143
;; TODO: Gelbooru support

;; (s/split "1girl apron bangs black_dress blood blood_on_clothes broom brown_eyes bucket carpet corpse dress from_above glass green_eyes hand_on_hip holding holding_broom indoors juliet_sleeves long_hair long_sleeves maid maid_apron pantyhose puffy_sleeves red_hair shoes smile standing waist_apron water white_legwear wooden_floor" #"\s+")
;; (s/split "" #"\s+")
;; it's only return a json and next and prev
;; https://www.npmjs.com/package/sankaku-api?activeTab=explore
;; https://capi-v2.sankakucomplex.com/posts/keyset
(defn get-metadata-sankaku [md5]
  (get-metadata {:name :sankaku
                 :url "https://capi-v2.sankakucomplex.com/posts/keyset"
                 :param {:tags (str "md5:" md5)}
                 :preprocess (fn [content]
                               (let [get-original #(first (:data %))
                                     original (get-original content)
                                     type-map {:0 :general
                                               :1 :artist
                                               :3 :copyright
                                               :4 :character
                                               :8 :medium ;; highres/white background
                                               :9 :meta ;; tagme
                                               }
                                     type-cvt (fn [t] (get type-map (keyword (str t)) :other))
                                     tags (map (fn [t] {:name (:tagName t) :type (type-cvt (:type t))})
                                               (:tags original))
                                     tags-list (reduce
                                                (fn [acc {:keys [name type]}]
                                                  (assoc acc type (conj (get acc type) name)))
                                                {} tags)]
                                 {:original original
                                  :final (merge tags-list {:id (:id original)
                                                           :rating (:rating original)
                                                           :md5 (:md5 original)})}))}))

;; (defn get-metadata-sankaku-by-id [id]
;;   (get-metadata {:name :sankaku
;;                  :url "https://capi-v2.sankakucomplex.com/posts/keyset"
;;                  :param {:tags (str "id:" id)}
;;                  :preprocess #(first (:data %))}))
;; (get-metadata-sankaku-by-id "32643440")

;; why the tags is placed at outside?
(defn get-metadata-yandere [md5]
  (get-metadata {:name :yandere
                 :url "https://yande.re/post.json"
                 :param {:api_version 2 :tags (str "md5:" md5) :include_tags 1}
                 :preprocess (fn [content]
                               (let [get-original
                                     (fn [content] (let [post (first (:posts content))
                                                         tags (:tags content)]
                                                     (if (some? post) (assoc post :tags tags) nil)))
                                     get-type (fn [tags type] (for [[tag t] tags :when (= t type)] tag))
                                     original (get-original content)
                                     ;; we only get one image if we specify md5, so the tags must belong to the image we retrieve
                                     get-final (fn [original] {:rating (:rating original)
                                                               :artist (get-type (:tags original) "artist")
                                                               :copyright (get-type (:tags original) "copyright")
                                                               :character (get-type (:tags original) "character")
                                                               :group (get-type (:tags original) "circle")
                                                               :general (get-type (:tags original) "general")
                                                               :md5 (:md5 original)
                                                               :id (:id original)})]
                                 (if (some? original) {:original original :final (get-final original)} nil)))}))


(def files (glob "/Volumes/Untitled 1/Grabber" (make-pattern exts)))
(cats-md5 files)

(get-metadata-yandere "b0c35b7124b721319911ebc1d03b85e4")
(get-metadata-sankaku "f6f3fc979c1609372e491d101ba51f09")
@(get-metadata-danbooru "f6f3fc979c1609372e491d101ba51f09")
