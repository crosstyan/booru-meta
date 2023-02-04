(ns booru-meta.booru
  "Get metadata from booru sites.
 I only plan to support one image query by md5 or post id (Maybe pixiv pid too).
"
  (:require [typed.clojure :as t]
            [clj-http.client :as client]
            [clojure.string :as s]
            [bites.core]
            [clojure.core.async :as a]
            [malli.core :as m]
            [booru-meta.schema :as schema])
  (:import (java.nio.file FileSystems)
           org.apache.commons.codec.binary.Hex)
  (:use [booru-meta.utils]
        [booru-meta.common]))

(defn guess-query [postid-or-md5]
  (cond
    (md5? postid-or-md5) (str "md5:" postid-or-md5)
    (int-only? postid-or-md5) (str "id:" postid-or-md5)
    :else nil))

(defn mk-query [query is-custom]
  (if is-custom
    (cond (string? query) query
          (map? query) (map->query query)
          :else nil)
    (guess-query query)))

;; https://github.com/dakrone/clj-http#basic-auth
;; https://danbooru.donmai.us/wiki_pages/help:api
;; %3A = ":"
(defn get-metadata
  "`preprocess` is expected to return a map with `:data` or `:error` keys.
   And the `data` field should contain a map with `:original` key and `:final` key and an optional `:extra` key.

   - `:original` is the original data returned from the booru. (minimally processed)
   - `:final` is the processed data.
   - `:extra` is the extra data that is not part of the `data` but still important (like API remaining or header).

  `:final` schema:
  ```clojure
  {:id String
  :rating String
  :md5 String
  :general (Seq string)
  :artist (Seq string)
  :copyright (Seq string)
  :character (Seq string)
  :meta (Seq string)
  }
```
   "
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
                                wrapped (merge processed {:source source})]
                            (deliver ret wrapped)))
                (fn [err] (deliver ret {:source source :error err})))
    ret))

(defn danbooru
  ([postid-or-md5] (danbooru postid-or-md5 {}))
  ([query opts]
   (let [is-custom (:custom-query opts)
         q (mk-query query is-custom)
         preprocess
         (fn [content]
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
               {:error :no-match})))
         mk-opt #(do {:name :danbooru
                      :url "https://danbooru.donmai.us/posts.json"
                      :param {:tags %}
                      :preprocess preprocess})]
     (if q
       (get-metadata (mk-opt q))
       {:error :invalid-query :query q}))))

;; https://gelbooru.com/index.php?page=wiki&s=view&id=18780
;; Have to use General csv/h5 database to get the metadata of tags. 
;; https://github.com/slpkbt/gelbooru-api/blob/37353af533946062dcb9cb0bacd2c984e5471991/index.js#L62
;; https://github.com/rainyDayDevs/pygelbooru/blob/c0b443aa1fa11e85a43968968ebf244a1bf5c55c/pygelbooru/gelbooru.py#L143
;; TODO: Gelbooru support

;; it's only return a json and next and prev
;; https://www.npmjs.com/package/sankaku-api?activeTab=explore
;; https://capi-v2.sankakucomplex.com/posts/keyset
(defn sankaku
  ([postid-or-md5] (sankaku postid-or-md5 {}))
  ([query opts]
   (let [is-custom (:custom-query opts)
         q (mk-query query is-custom)
         preprocess
         (fn [content]
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
               {:error :no-match})))
         mk-opt #(do
                   {:name :sankaku
                    :url "https://capi-v2.sankakucomplex.com/posts/keyset"
                    :param {:tags %}
                    :preprocess preprocess})]
     (if q
       (get-metadata (mk-opt q))
       {:error :invalid-query :query q}))))

;; why the tags is placed at outside?
(defn yandere
  ([postid-or-md5] (yandere postid-or-md5 {}))
  ([query opts]
   (let [custom-query (:custom-query opts)
         q (mk-query query custom-query)
         preprocess
         (fn [content]
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
               {:error :no-match})))
         mk-opt #(do {:name :yandere
                      :url "https://yande.re/post.json"
                      :param {:api_version 2 :tags % :include_tags 1}
                      :preprocess preprocess})]
     (if q
       (get-metadata (mk-opt q))
       {:error :invalid-query :query q}))))

(defn link->source [link]
  (cond
    ;; https://danbooru.donmai.us/posts/3520001
    ;; https://danbooru.donmai.us/post/3520001 (invalid link)
    ;; https://danbooru.donmai.us/post/show/3520001
    (some? (re-find #"danbooru" link)) {:source :danbooru :id (last (re-find #"posts?/(show/)?(\d+)" link))}
    (some? (re-find #"sankaku" link)) {:source :sankaku :id (last (re-find #"posts?/(show/)?(\d+)" link))}
    (some? (re-find #"yande.re" link)) {:source :yandere :id (last (re-find #"posts?/(show/)?(\d+)" link))}
    ;; https://www.pixiv.net/member_illust.php?mode=medium&illust_id=70534730
    ;; https://www.pixiv.net/artworks/70534730
    (some? (re-find #"pixiv" link)) {:source :pixiv :id (last (filter some? (re-find #"artworks/(\d+)|illust_id=(\d+)" link)))}
    :else nil))

(defn sauce->booru
  "Return a function that can be used to get metadata from sauce result. 
   If the sauce is invalid, return `:invalid`. 
   If the sauce is valid but no any sites match, return `:no-match`.
   
   Tips: use `fn?` or `keyword?` to check if the return value is a function or not."
  [result]
  (if (m/schema schema/sauce-result result)
    (let [f (get-in result [:data :final])
          l (map #(assoc % :src (link->source (:link %))) f)
          l' (filter #(some? (:src %)) l)
          ;; I assume the result is sorted
          r (first l')
          s (get-in r [:src :source])
          id (get-in r [:src :id])]
      (if (some? id)
        (cond (= s :danbooru) #(danbooru id)
              (= s :sankaku) #(sankaku id)
              (= s :yandere) #(yandere id)
              (= s :pixiv) #(danbooru {:pixiv id} {:custom-query true})
              :else :no-match)
        :no-match))
    :invalid-sauce))

