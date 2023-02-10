(ns booru-meta.utils
  (:require [babashka.fs :as fs]
            [bites.core]
            [booru-meta.schema :as schema]
            [cheshire.core :as json]
            [clojure.core.async :as a]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.walk :as walk]
            [malli.core :as m]
            [mikera.image.core :as imagez]
            [typed.clojure :as t])
  (:import (java.nio.file FileSystems)
           org.apache.commons.codec.binary.Hex))

;; okay you have to use two stars (idk why)
(defn glob
  "See also `babashka.fs/glob`. Just a redundant implementation."
  [path pattern]
  (let [files (file-seq (io/file path))
        java-fs (java.nio.file.FileSystems/getDefault)
        matcher (.getPathMatcher java-fs (str "glob:" pattern))]
    (filter (fn [file]
              (let [path (.toPath file)]
                (.matches matcher path)))
            files)))
(t/ann glob [t/Str t/Str :-> (t/Seq java.io.File)])

(defn make-pattern
  "make glob pattern to capture list of extensions."
  [exts]
  (str "**.{"
       (s/join "," exts)
       "}"))
(def image-glob-pattern (make-pattern ["jpg" "jpeg" "png" "webp"]))

(defmulti md5?
  "Check if string or filename is md5 hash."
  type)
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

(defn str->bytes
  "Convert string to byte array."
  ([^String s]
   (str->bytes s "UTF-8"))
  ([^String s, ^String encoding]
   (.getBytes s encoding)))

(defn hex->bytes
  "Convert hexadecimal encoded string to bytes array."
  [^String data]
  (Hex/decodeHex (.toCharArray data)))

(defn categorize-by-md5
  "split files into two groups: md5 and normal"
  [files]
  (let [files-with-type (map (fn [path] {:file path :type (if (md5? path) :md5 :normal)}) files)]
    (reduce (fn [acc {:keys [file type]}]
              (assoc acc type (conj (get acc type) file)))
            {} files-with-type)))

(defn get-new-size
  ([old-size] (get-new-size old-size {}))
  ([old-size & {:keys [new-length is-preserve-long] :or {new-length 512 is-preserve-long true}}]
   (let [[old-w old-h] old-size
         new-l new-length
         preserve-long? is-preserve-long]
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

;; can only use png before convert to rgb
;; TODO: convert it to rgb then compresses it to jpeg
;; https://stackoverflow.com/questions/44182400/how-to-convert-bufferedimage-rgba-to-bufferedimage-rgb
;; https://github.com/mikera/imagez/issues/33
(defn read-compress-img
  "Read a file and compress it with its short side 512 to png format.

 `java.io.File :-> ByteArray`"
  [file]
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

(def ByteArray
  "ByteArray is the class name of a byte array."
  (class (byte-array [0x00 0xff])))

(defn byte-array? [ba]
  ;; create a byte array and get it type
  (instance? ByteArray ba))

(defn split-kv-string
  "Split string `s` by `:` whose key is start with captial letter 
 and return a map of key value pairs."
  [s] (try
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

(defn interval
  "apply function `f` with `args` every `mills` milliseconds.
 Returns a function that can be called to stop the interval.

 See also: https://stackoverflow.com/questions/41283956/start-and-stop-core-async-interval"
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

(defn with-extension
  "`path` should be coherent with `babashka.fs/path`. `ext` could contain a leading dot (or not)."
  [path ext]
  (let [path (fs/path path)
        clean-ext (s/replace ext #"^\." "")
        new-path (.resolveSibling path (str (file->stem path) "." clean-ext))]
    new-path))

(defn bytes->string
  "Convert bytes array to string."
  [bytes] (s/join (map #(format "%02x" %) bytes)))

(defn calc-md5
  "Calculate md5 of a file.
 
 `java.io.File :-> ByteArray`. See also `bytes->string`"
  [file]
  (let [md (java.security.MessageDigest/getInstance "MD5")
        fis (java.io.FileInputStream. file)]
    ;; copilot wrote this
    (loop [read (.read fis)]
      (when (not= -1 read)
        (.update md (byte-array [(int read)]))
        (recur (.read fis))))
    (.digest md)))

(defmulti int-only?
  "Check if a string is a valid POSITIVE integer."
  type)
(defmethod int-only? Integer [n]
  (>= n 0))
(defmethod int-only? Long [n]
  (>= n 0))
(defmethod int-only? BigInteger [n]
  (>= n 0))
(defmethod int-only? String [s]
  (some? (re-find #"^\d+$" s)))
(defmethod int-only? :default [s]
  (int-only? (str s)))

(defn rand-int-range [a b]
  (+ a (rand-int (- b a))))

(defn map->query [m]
  (let [query (map (fn [[k v]] (str (name k) ":" v)) m)]
    (s/join " " query)))

(def chan-type (type (a/chan)))
(defn chan? [x]
  (instance? chan-type x))

(defn filter-out-nomatch [file]
  (not (fs/exists? (with-extension file ".nomatch.json"))))

(defn filter-out-matched [file]
  (not (fs/exists? (with-extension file ".json"))))


(defn get-sauced-no-booru
  "Give a json or picture"
  [file]
  (let [check (fn [file]
                (let [c (-> file str slurp json/decode walk/keywordize-keys)]
                  (if (m/validate schema/persistent-info c)
                    (let [ks (keys (:data c))
                          sauce [:sauce :iqdb :ascii2d]
                          booru [:danbooru :gelbooru :sankaku :yandere]]
                      (reduce #(and %1 %2) true (map (fn [k] (and (some #(= % k) sauce) (not-any? #(= % k) booru)))  ks)))
                    false)))
        json-file (with-extension file ".json")]
    (if (fs/exists? json-file)
      (check json-file)
      false)))

(defn filter-out-folders [file folders]
  (let [parent (fs/parent file)
        name (file->stem parent)]
    (not (some #(= name %) folders))))
