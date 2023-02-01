(ns booru-meta.core
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [typed.clojure :as t]
            [clj-http.client :as client]
            [progrock.core :as pr]
            [cheshire.core :as json]
            [clojure.string :as s]
            [hickory.select :as hs]
            [mikera.image.core :as imagez]
            [hickory.core :as html]
            [bites.core]
            [clojure.core.async :as a])
  (:import (java.nio.file FileSystems)
           org.apache.commons.codec.binary.Hex))

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

(defn file->stem [file]
  (com.google.common.io.Files/getNameWithoutExtension (str file)))

(defmethod md5? java.io.File [file]
  (let [stem (file->stem file)
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
(defn get-metadata
  "`preprocess` is expected to return a map with `:data` or `:error` keys."
  [options]
  (let [url (:url options)
        header {"User-Agent" user-agent}
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
                                   {:data
                                    {:original original
                                     :final (merge tags-list {:id (:id original)
                                                              :rating (:rating original)
                                                              :md5 (:md5 original)})}}))})
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

(def files (glob "/Volumes/Untitled 1/Grabber" (make-pattern exts)))
(cats-md5 files)

(fs/relativize (fs/path "/Volumes/Untitled 1/Grabber") (fs/path "/Volumes/Untitled 1/Grabber/nashiko_(nanaju_ko)/5222d5324f68d6a5693e6864d3d1e9ab.jpg"))
(fs/normalize (fs/path "/Volumes/Untitled 1/Grabber"))
;; https://stackoverflow.com/questions/33273341/java-nio-how-to-add-extension-to-an-absolute-path
(.resolveSibling (fs/path "/Volumes/Untitled 1/Grabber/nashiko_(nanaju_ko)/5222d5324f68d6a5693e6864d3d1e9ab.jpg") "123")
(fs/path "/Volumes/Untitled 1/Grabber/nashiko_(nanaju_ko)/5222d5324f68d6a5693e6864d3d1e9ab.jpg")
(file->stem (fs/path "/Volumes/Untitled 1/Grabber/nashiko_(nanaju_ko)/5222d5324f68d6a5693e6864d3d1e9ab.jpg"))
(.getFileName (fs/path "/Volumes/Untitled 1/Grabber/nashiko_(nanaju_ko)/5222d5324f68d6a5693e6864d3d1e9ab.jpg"))

(defn with-extension [path ext]
  (let [path (fs/path path)
        clean-ext (s/replace ext #"^\." "")
        new-path (.resolveSibling path (str (file->stem path) "." clean-ext))]
    new-path))
;; (with-extension (io/file "/Volumes/Untitled 1/Grabber/nashiko_(nanaju_ko)/5222d5324f68d6a5693e6864d3d1e9ab.jpg") "txt")

(get-metadata-yandere "b0c35b7124b721319911ebc1d03b85e4")
(get-metadata-sankaku "f6f3fc979c1609372e491d101ba51f09")
(get-metadata-danbooru "f6f3fc979c1609372e491d101ba51f09")

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
     (fn [m [k v]] (assoc m k v)) {}
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
        sims (map (comp #(do {:sim %})
                        percentage-to-num
                        #(re-find #"[0-9]+\%" %) first
                        #(get % :content) first
                        (fn [el] (hs/select (hs/find-in-text #"similarity") el))) tbodies)
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
                                       (if (some? data) (deliver ret {:data data :source source})
                                           (deliver ret {:error :no-match :source source}))))
                                   (fn [error] (deliver ret {:error error :source source})))
                       (deliver ret {:error :no-match :source source}))))
                 (fn [error] (deliver ret {:error error :source source})))
    ret))

;; pay attention to short_remaining and long_remaining
;; if short_remaining is 0, wait for 30 seconds
(get-sauce (io/file "/Volumes/Untitled 1/Grabber/maachi/90375559_p0 - 無題.jpg") {:api-key "009934e06a88a3a1f28c565d69a5273ee47008e1"})

;; https://github.com/clj-commons/hickory
(get-iqdb (io/file "/Volumes/Untitled 1/Grabber/kazuharu_kina/33767cc3b60dcebb3733854dd03b7da5.jpg") {:3d? false})
(get-ascii2d (io/file "/Volumes/Untitled 1/Grabber/kazuharu_kina/33767cc3b60dcebb3733854dd03b7da5.jpg") {})

(defn calc-md5 [file]
  (let [md (java.security.MessageDigest/getInstance "MD5")
        fis (java.io.FileInputStream. file)]
    ;; copilot wrote this
    (loop [read (.read fis)]
      (when (not= -1 read)
        (.update md (byte-array [(int read)]))
        (recur (.read fis))))
    (.digest md)))

;; https://stackoverflow.com/questions/10062967/clojures-equivalent-to-pythons-encodehex-and-decodehex
(defn bytes->string [bytes]
  (s/join (map #(format "%02x" %) bytes)))

((comp bytes->string calc-md5) (io/file "/Volumes/Untitled 1/Grabber/kazuharu_kina/33767cc3b60dcebb3733854dd03b7da5.jpg"))
;; (-> (io/file "/Volumes/Untitled 1/Grabber/kazuharu_kina/33767cc3b60dcebb3733854dd03b7da5.jpg")
;;     (calc-md5)
;;     (bytes->string))



(def semaphore (atom 0))

;; https://stackoverflow.com/questions/41283956/start-and-stop-core-async-interval
;; https://github.com/clojure/core.async/blob/master/examples/walkthrough.clj
(defn interval
  "apply function `f` with `args` every `mills` milliseconds.
 Returns a function that can be called to stop the interval."
  [f args mills]
  (let [timimg (a/chan)
        kickoff #(a/go
                   (a/<! (a/timeout mills))
                   (a/>! timimg true))]
    (a/go-loop []
      (when (a/<! timimg)
        (a/go (apply f args))
        (kickoff)
        (recur)))
    (kickoff)
    #(a/close! timimg)))

;; https://stackoverflow.com/questions/25948511/when-to-use-if-and-when-in-clojure
(defn run-batch
  "`short-limit` should be atom."
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
                         info @(get-metadata-danbooru stem)
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

;; (fs/write-lines (fs/path "/Users/crosstyan/Code/test.txt")  ["test"])

(def c (run-batch (take 30 (:md5 (cats-md5 files))) {:max-limit 30}))
(c)

(def long-str "
02 07 06 07 06 07 06 07 06 07 06 07 06 07 06 07 06 07 06 07 06 07 06 07 06 07 06
FF FF FF FF 0F 1F 1F 1F 1F 1F 1F 1F 1F 1F 1F 1F 1F 1F 1F 1F 1F 1F 1F 1F 1F 1F 1F
1F 1F 1F 1F FF FF FF FF 0D 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01
01 01 01 01 01 01 01 01 FF FF FF FF 0C 01 01 01 01 01 01 01 01 01 01 01 01 01 01
01 01 01 01 01 01 01 01 01 01 01 01 FF FF FF FF 0E 09 09 09 09 09 09 09 09 09 0A
09 09 09 09 09 09 09 09 09 09 09 0A 09 09 09 09 FF FF FF FF 00 0A 1E 1A 
              ")

(defn str->bytes
  "Convert string to byte array."
  ([^String s]
   (str->bytes s "UTF-8"))
  ([^String s, ^String encoding]
   (.getBytes s encoding)))
(str->bytes long-str)

(defn hex->bytes
  "Convert hexadecimal encoded string to bytes array."
  [^String data]
  (Hex/decodeHex (.toCharArray data)))

(s/join ", " (map #(format "0x%02x" %) (hex->bytes (s/replace  long-str #"\s+" ""))))
