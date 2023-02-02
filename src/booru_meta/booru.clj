(ns booru-meta.booru
  (:require [typed.clojure :as t]
            [clj-http.client :as client]
            [clojure.string :as s]
            [bites.core]
            [clojure.core.async :as a])
  (:import (java.nio.file FileSystems)
           org.apache.commons.codec.binary.Hex)
  (:use [booru-meta.utils]
        [booru-meta.common]))


;; https://github.com/dakrone/clj-http#basic-auth
;; https://danbooru.donmai.us/wiki_pages/help:api
;; %3A = ":"
;; site option
;; {:name :danbooru
;;  :url "https://danbooru.donmai.us/posts.json"
;;  :make-param (fn [md5] {:tags (str "md5:" md5)})
;;  :preprocess (fn [content] (if (seq? content) nil (first content)))}
(defn get-metadata
  "`preprocess` is expected to return a map with `:data` or `:error` keys."
  [options]
  (let [url (:url options)
        ua (if-let [u (:user-agent options)] u default-user-agent)
        header {"User-Agent" ua}
        preprocess (:preprocess options)
        param (:param options)
        source (:name options)
        ret (promise)]
    (client/get url {:headers header :content-type :json :as :json :query-params param :async true}
                (fn [res] (let [processed (preprocess (:body res))
                                wrapped (if (some? processed) {:data (:data processed) :source source} {:data nil :source source :error :empty})]
                            (deliver ret wrapped)))
                (fn [err] (deliver ret {:data nil :source source :error err})))
    ret))

(defn get-metadata-danbooru [md5]
  (if (md5? md5)
    (get-metadata {:name :danbu
                   :url "https://danbooru.donmai.us/posts.json"
                   :param {:tags (str "md5:" md5)}
                   :preprocess (fn [content]
                                 (let [original (if (seq? content) nil (first content))
                                       split (fn [s] (if (and (= (type s) String) (not= s "")) (s/split s #"\s+") []))]
                                   (if (some? original)
                                     {:data
                                      {:original original
                                       :final {:id (:id original)
                                               :rating (:rating original)
                                               :md5 (:md5 original)
                                               :general (split (:tag_string_general original))
                                               :artist (split (:tag_string_artist original))
                                               :copyright (split (:tag_string_copytright original))
                                               :character (split (:tag_string_character original))
                                               :meta (split (:tag_string_meta original))}}}
                                     {:error :no-match})))})
    {:error :not-md5}))

;; https://gelbooru.com/index.php?page=wiki&s=view&id=18780
;; Have to use General csv/h5 database to get the metadata of tags. 
;; https://github.com/slpkbt/gelbooru-api/blob/37353af533946062dcb9cb0bacd2c984e5471991/index.js#L62
;; https://github.com/rainyDayDevs/pygelbooru/blob/c0b443aa1fa11e85a43968968ebf244a1bf5c55c/pygelbooru/gelbooru.py#L143
;; TODO: Gelbooru support

;; it's only return a json and next and prev
;; https://www.npmjs.com/package/sankaku-api?activeTab=explore
;; https://capi-v2.sankakucomplex.com/posts/keyset
(defn get-metadata-sankaku [md5]
  (if (md5? md5)
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
                                       type-cvt #(get type-map (keyword (str %)) :other)
                                       tags (map #(do {:name (:tagName %) :type (type-cvt (:type %))})
                                                 (:tags original))
                                       tags-list (reduce
                                                  (fn [acc {:keys [name type]}]
                                                    (assoc acc type (conj (get acc type) name)))
                                                  {} tags)]
                                   (if (some? original)
                                     {:data
                                      {:original original
                                       :final (merge tags-list {:id (:id original)
                                                                :rating (:rating original)
                                                                :md5 (:md5 original)})}}
                                     {:error :no-match})))})
    {:error :not-md5}))

;; (defn get-metadata-sankaku-by-id [id]
;;   (get-metadata {:name :sankaku
;;                  :url "https://capi-v2.sankakucomplex.com/posts/keyset"
;;                  :param {:tags (str "id:" id)}
;;                  :preprocess #(first (:data %))}))
;; (get-metadata-sankaku-by-id "32643440")

;; why the tags is placed at outside?
(defn get-metadata-yandere [md5]
  (if (md5? md5)
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
                                   (if (some? original)
                                     {:data {:original original :final (get-final original)}}
                                     {:error :no-match})))})
    {:error :not-md5}))
