(ns booru-meta.core
  (:require [clojure.java.io :as io]
            [typed.clojure :as t]
            [clj-http.client :as client]
            [progrock.core :as pr]
            [cheshire.core :as json]
            [clojure.string :as s]
            [hickory.select :as hs]
            [mikera.image.core :as imagez]
            [hickory.core :as html])
  (:import (java.nio.file FileSystems)))

;; https://github.com/babashka/fs
;; https://stackoverflow.com/questions/5019978/what-is-the-most-concise-clojure-equivalent-for-rubys-dir-glob
(def exts ["jpg" "jpeg" "png" "webp"])

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
       (s/join "," exts)
       "}"))

(defmulti md5? type)
(defmethod md5? :default [_x] false)
(defmethod md5? String [s]
  (if (not= (count s) 32)
    false
    (some? (re-find #"^[0-9a-f]+$" s))))
(defmethod md5? java.io.File [file]
  (let [stem (com.google.common.io.Files/getNameWithoutExtension (str file))
        stem (s/lower-case stem)]
    (md5? stem)))

(defn cats-md5
  [files]
  (let [files-with-type (map (fn [path] {:file path :type (if (md5? path) :md5 :normal)}) files)]
    (reduce (fn [acc {:keys [file type]}]
              (assoc acc type (conj (get acc type) file)))
            {} files-with-type)))

(def user-agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.71 Safari/537.36")

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
    (client/get url {:headers header :content-type :json :as :json :query-params param :async true}
                (fn [res] (let [processed (preprocess (:body res))
                                wrapped (if (some? processed) {:data processed :source source :error nil} {:data nil :source source :error :empty})]
                            (deliver ret wrapped)))
                (fn [err] (deliver ret {:data nil :source source :error err})))
    ret))

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

(defn get-new-size
  ([old-size] (get-new-size old-size {}))
  ([old-size options]
   (let [[old-w old-h] old-size
         new-l (get options :new-length 512)
         preserve-long? (get options :preserve-long true)]
     (if (or (< old-w new-l) (< old-h new-l))
       old-size
       (if preserve-long?
         (if (> old-w old-h)
           (let [new-h new-l
                 new-w (int (* new-l (/ old-w old-h)))]
             [new-w new-h])
           (let [new-w new-l
                 new-h (int (* new-l (/ old-h old-w)))]
             [new-w new-h]))
         (if (> old-w old-h)
           (let [new-w new-l
                 new-h (int (* new-l (/ old-h old-w)))]
             [new-w new-h])
           (let [new-h new-l
                 new-w (int (* new-l (/ old-w old-h)))]
             [new-w new-h])))))))

;; java.io.File :-> ByteArray
;; https://guava.dev/releases/19.0/api/docs/com/google/common/io/FileBackedOutputStream.html
;; https://stackoverflow.com/questions/44182400/how-to-convert-bufferedimage-rgba-to-bufferedimage-rgb
;; can only use png before convert to rgb
;; https://github.com/mikera/imagez/issues/33
(defn read-compress-img [file]
  (let [image (imagez/load-image (str file))
        w (imagez/width image)
        h (imagez/height image)
        [w' h'] (get-new-size [w h] {:preserve-long false})
        new-image (imagez/resize image w' h')
        buf (new java.io.ByteArrayOutputStream)
        _writer (imagez/write new-image buf "png" {:quality 0.8})
        bytes (.toByteArray buf)]
    (.close buf)
    bytes))

;; https://github.com/kitUIN/PicImageSearch/blob/3c7f13cab5d49d38f6236f8f26f54a2e64d06175/PicImageSearch/saucenao.py#L27
(defn get-sauce [file options]
  (let [body (read-compress-img file)
        api-key (:api-key options)
        ret (promise)
        url "https://saucenao.com/search.php"
        source :saucenao
        params {:output_type 2
                :api_key api-key
                :minsim 60
                :testmode 0}]
    (if (some? api-key)
      (client/post url {:async true :multipart [{:name "file" :content body}] :query-params params :as :json}
                   (fn [response]
                     (deliver ret {:data (:body response) :source source}))
                   (fn [error] (deliver ret {:error error :source source})))
      (deliver ret {:error :no-api-key}))
    ret))

(defn split-kv-string [s]
  (try
    (reduce
     (fn [m [k v]] (assoc m k v))
     {}
     (map #(let [[k v] (s/split % #":")
                 k ((comp keyword s/lower-case s/trim) k)
                 v (s/trim v)]
             [k v])
          ;; split by capturing any string that starts with a capital letter,
          ;; followed by a colon, followed by any string that doesn't start with a
          ;; capital letter
          (re-seq #"[A-Z][^:]*:\s*[^[A-Z]]*" s)))
    (catch Exception _e {})))

;; iqdb will return a html page. Need to parse it.
;; You can't match a list. Have to use map or first.
(defn extract-iqdb-info [iqdb-raw-html-string]
  (let [h (html/as-hickory (html/parse iqdb-raw-html-string))
        nomatch (hs/select (hs/child (hs/class "nomatch")) h)
        ;; leave the final item out. It's just a checkbox
        tbodies (drop-last (hs/select (hs/child (hs/and (hs/tag :tbody) (hs/has-descendant (hs/tag :a)))) h))
        ex-a (fn [e] (let [alt (get-in (first (:content e)) [:attrs :alt])
                           m {:link (get-in e [:attrs :href])}]
                       (assoc m :meta (split-kv-string alt))))
        percentage-to-num (fn [p] (float (/ (Integer/parseInt (re-find #"[0-9]+" p)) 100)))
        sims (map (comp (fn [p] {:sim p})
                        percentage-to-num
                        #(re-find #"[0-9]+\%" %) first
                        #(get % :content) first
                        (fn [e] (hs/select (hs/find-in-text #"similarity") e))) tbodies)
        links (map (comp
                    (fn [m] (assoc m :link (if (s/starts-with? (:link m) "//") ;; add "https:" to the link
                                             (str "https:" (:link m)) (:link m))))
                    ex-a first
                    (fn [e] (hs/select (hs/descendant (hs/tag :a)) e))) tbodies)]
    (if (seq nomatch) nil (map #(merge %1 %2) sims links))))

(defn get-iqdb [file options]
  (let [body (read-compress-img file)
        three-d? (if (some? (:3d? options)) (:3d? options) false)
        url (if three-d? "https://3d.iqdb.org/" "https://iqdb.org/")
        source :iqdb
        ret (promise)]
    (client/post url {:async true :multipart [{:name "file" :content body}] :as :auto :headers {"User-Agent" user-agent}}
                 (fn [response]
                   (let [data (extract-iqdb-info (:body response))]
                     (deliver ret (if (some? data) {:data data :source source} {:error :no-match}))))
                 (fn [error] (deliver ret {:error error :source source})))
    ret))

(defn extract-ascii2d-info [raw-html-string]
  (let [hp (html/as-hickory (html/parse raw-html-string))
        get-pair (fn [[work author]]
                   {:work {:name (first (:content work))
                           :link (get-in work [:attrs :href])}
                    :author {:name (first (:content author))
                             :link (get-in author [:attrs :href])}})]
    (map get-pair
         (filter #(= (count %) 2)
                 (map (comp #(hs/select (hs/descendant (hs/tag :a)) %) first
                            #(hs/select (hs/descendant (hs/class "detail-box")) %))
                      (hs/select (hs/child (hs/class "item-box")) hp))))))

(defn get-ascii2d [file options]
  (let [body (read-compress-img file)
        url "https://ascii2d.obfs.dev/"
        append-url (fn [url path] (let [uri (java.net.URI/create url)]
                                    (str (.resolve uri path))))
        is-bovw (if (some? (:bovw? options)) (:bovw? options) false)
        source :ascii2d
        ret (promise)]
    (client/post (append-url url "/search/file") {:async true :multipart [{:name "file" :content body}] :as :auto :headers {"User-Agent" user-agent}}
                 (fn [response]
                   (let [new-url (get-in response [:headers "Location"])
                         new-url (if is-bovw (s/replace-first new-url #"\/color\/" "/bovw/") new-url)]
                     (if (some? new-url)
                       (client/get new-url {:async true :as :auto :headers {"User-Agent" user-agent}}
                                   (fn [response]
                                     (let [data (extract-ascii2d-info (:body response))]
                                       (if (some? data) (deliver ret {:data data :source source}) (deliver ret {:error :no-match}))))
                                   (fn [error] (deliver ret {:error error :source source})))
                       (deliver ret {:error :no-match}))))
                 (fn [error] (deliver ret {:error error :source source})))
    ret))

;; pay attention to short_remaining and long_remaining
;; if short_remaining is 0, wait for 30 seconds
@(get-sauce (io/file "/Volumes/Untitled 1/Grabber/maachi/90375559_p0 - 無題.jpg") {:api-key "009934e06a88a3a1f28c565d69a5273ee47008e1"})

;; https://github.com/clj-commons/hickory
@(get-iqdb (io/file "/Volumes/Untitled 1/Grabber/kazuharu_kina/33767cc3b60dcebb3733854dd03b7da5.jpg") {:3d? false})
@(get-ascii2d (io/file "/Volumes/Untitled 1/Grabber/kazuharu_kina/33767cc3b60dcebb3733854dd03b7da5.jpg") {})

;; (re-find #"[A-Z][^:]*:\s*[^[A-Z]]*" "Rating: g Score: 2 Tags: 1girl backpack bag blue_bow blue_skirt bow brown_bag brown_footwear brown_hair closed_eyes collarbone eyebrows_visible_through_hair green_background hair_ribbon highres kazuharu_kina open_mouth original ponytail ribbon school_uniform serafuku simple_background skirt sleeveless smile solo teeth tongue uniform upper_teeth yellow_ribbon")

;; (let [[k v] (s/split  "Rating: safe" #":")]
;;   (print k v))