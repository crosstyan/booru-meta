(ns booru-meta.playground
  (:require [babashka.fs :as fs]
            [bites.core]
            [booru-meta.sauce :as sauce]
            [booru-meta.schema :as schema]
            [clojure.core.async :as a]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [java-time.api :as jt]
            [malli.core :as m]
            [progrock.core :as pr]
            [cheshire.core :as json]
            [booru-meta.booru :as booru])
  (:use [booru-meta.utils]
        [booru-meta.sauce]
        [booru-meta.booru]
        [booru-meta.core]))


(def files (glob "/Volumes/Untitled 1/Grabber" image-glob-pattern))
(def jsons (glob "/Volumes/Untitled 1/Grabber" (make-pattern ["json"])))
(doseq [file jsons]
  (fs/read-all-lines file))

(categorize-by-md5 files)

;; (def cancel (run-batch (shuffle (:md5 (categorize-by-md5 files))) {:max-limit 20 :reset-interval-ms 25000 :root-path "/Volumes/Untitled 1/Grabber" :random-delay-ms [100 400]}))
;; (cancel)

(fs/relativize (fs/path "/Volumes/Untitled 1/Grabber") (fs/path "/Volumes/Untitled 1/Grabber/nashiko_(nanaju_ko)/5222d5324f68d6a5693e6864d3d1e9ab.jpg"))
(fs/normalize (fs/path "/Volumes/Untitled 1/Grabber"))
;; https://stackoverflow.com/questions/33273341/java-nio-how-to-add-extension-to-an-absolute-path
(.resolveSibling (fs/path "/Volumes/Untitled 1/Grabber/nashiko_(nanaju_ko)/5222d5324f68d6a5693e6864d3d1e9ab.jpg") "123")
(fs/path "/Volumes/Untitled 1/Grabber/nashiko_(nanaju_ko)/5222d5324f68d6a5693e6864d3d1e9ab.jpg")
(file->stem (fs/path "/Volumes/Untitled 1/Grabber/nashiko_(nanaju_ko)/5222d5324f68d6a5693e6864d3d1e9ab.jpg"))
(.getFileName (fs/path "/Volumes/Untitled 1/Grabber/nashiko_(nanaju_ko)/5222d5324f68d6a5693e6864d3d1e9ab.jpg"))
;; (with-extension (io/file "/Volumes/Untitled 1/Grabber/nashiko_(nanaju_ko)/5222d5324f68d6a5693e6864d3d1e9ab.jpg") "txt")

@(yandere "b0c35b7124b721319911ebc1d03b85e4")
@(yandere 1048103)
(sankaku "f6f3fc979c1609372e491d101ba51f09")
@(danbooru "f6f3fc979c1609372e491d101ba51f09")

@(danbooru {:pixiv 8314} {:is-custom true})
@(danbooru "0c0d79e53b5e50c568baea759194476a")

;; pay attention to short_remaining and long_remaining
;; if short_remaining is 0, wait for 30 seconds
(sauce (io/file "/Volumes/Untitled 1/Grabber/maachi/90375559_p0 - 無題.jpg") {:api-key "009934e06a88a3a1f28c565d69a5273ee47008e1"})

;; https://github.com/clj-commons/hickory
(iqdb (io/file "/Volumes/Untitled 1/Grabber/kazuharu_kina/33767cc3b60dcebb3733854dd03b7da5.jpg") {:3d? false})
(ascii2d (io/file "/Volumes/Untitled 1/Grabber/kazuharu_kina/33767cc3b60dcebb3733854dd03b7da5.jpg") {})

(def s0 @(iqdb (io/file "C:\\Users\\cross\\Desktop\\mt_o\\Artists\\holy_pumpkin\\1ede42428758e996b8a9d6fb757a1974.jpg")))

(def s1 @(sauce (io/file "C:\\Users\\cross\\Desktop\\mt_o\\Artists\\holy_pumpkin\\1ede42428758e996b8a9d6fb757a1974.jpg")  {:api-key "009934e06a88a3a1f28c565d69a5273ee47008e1"}))

(def s3 @(ascii2d (io/file "C:\\Users\\cross\\Desktop\\mt_o\\Artists\\holy_pumpkin\\1ede42428758e996b8a9d6fb757a1974.jpg")))


(def s0' @(iqdb (io/file "C:\\Users\\cross\\Desktop\\mt_o\\ArtistsNoEmb\\ruriri\\2rnc9_17.jpg")))

(m/explain schema/result s0')

@(danbooru "1ede42428758e996b8a9d6fb757a1974")

(query-by-md5-then-save (io/file "C:\\Users\\cross\\Desktop\\mt_o\\Artists\\holy_pumpkin\\1ede42428758e996b8a9d6fb757a1974.jpg"))
(query-by-file-then-save  (io/file "C:\\Users\\cross\\Desktop\\27252276_p0.jpg"))

(a/take! (query-by-md5-then-save (io/file "C:\\Users\\cross\\Desktop\\mt_o\\ArtistsNoEmb\\aya_shobon\\0e82d19881c311a2d6ddc905fc1e2f95.png"))
         #(println "done" %))

s1


(map #(if (empty? (:links %)) (:link %) (:links %)) (get-in s1 [:data :final]))

s3

(link->source "https://danbooru.donmai.us/posts/3243061")
(link->source "https://www.pixiv.net/artworks/70534730")
(link->source "https://www.pixiv.net/member_illust.php?mode=medium&illust_id=70534730")


(def f (sauce->booru s1))

;; (if (fn? f) @(f) f)

(m/validate schema/sauce-result s3)

;; https://stackoverflow.com/questions/10062967/clojures-equivalent-to-pythons-encodehex-and-decodehex

((comp bytes->string calc-md5) (io/file "/Volumes/Untitled 1/Grabber/kazuharu_kina/33767cc3b60dcebb3733854dd03b7da5.jpg"))
;; (-> (io/file "/Volumes/Untitled 1/Grabber/kazuharu_kina/33767cc3b60dcebb3733854dd03b7da5.jpg")
;;     (calc-md5)
;;     (bytes->string))



(def f
  (let [file "C:\\Users\\cross\\Desktop\\mt_o\\Artists\\luicent\\4043c08ebed607e621b82ec7fdb49413.json"]
    (-> file str slurp json/decode walk/keywordize-keys)))

(def files (let [files (:md5 (categorize-by-md5 (shuffle (glob (io/file "C:\\Users\\cross\\Desktop\\mt_o\\Artists") image-glob-pattern))))
                 files (filter filter-out-matched files)
                 files (filter filter-out-nomatch files)
                 ng-folders ["PVC" "orange@aigc" "kantoku@sketch" "wlop" "53928"]
                 files (filter #(filter-out-folders % ng-folders) files)]
             files))

(count files)


(def files (filter get-sauced-no-booru files))
(def queries (map get-sauce-query files))
(def fq (map vector files queries))
(first fq)

(count files)
(first files)
;; (nth files 1)

(first queries)

(count fq)

(def r (atom nil))
(a/take! ((:func (first queries))) #(do (reset! r %)
                                        (println "done" %)))

(a/take! (query-sauce-2-booru-then-save (first files) (:func (first queries)))
         #(do (reset! r %)
              (println "done" %)))
@r


(doseq [[file query] fq]
  (let [func (:func query)]
    (if (fn? func)
      (a/take! (query-sauce-2-booru-then-save file func)
               (fn [res]
                 (println "done" (str file) (true? (seq res)))))
      (println "not a function" (str file) (str queries)))))

(doseq [a [1 2 3 4 5 6 7 8 9 10]
        b ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j"]]
  (println a b))

(map vector [1 2 3 4 5 6 7 8 9 10] ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j"])

files
(count files)


;; https://stackoverflow.com/questions/11824134/caused-by-java-lang-outofmemoryerror-java-heap-space
(def cancel
  (let [file-list files
        {cancel :cancel failed-chan :failed-chan bar-chan :bar-chan}
        (run-batch file-list query-by-md5-then-save
                   :max-limit 6
                   :reset-interval-ms 10000
                   :root-path "C:\\Users\\cross\\Desktop\\mt_o\\"
                   :random-delay-ms [500 1000])
        {failed-bar-chan :bar-chan}
        (query-sauce-for-fails failed-chan)]
    (a/go-loop []
      (a/alt! [bar-chan failed-bar-chan]
              ([val _chan] (pr/print val)))
      (recur))
    #(do (cancel)
         (a/close! bar-chan)
         (when (chan? failed-chan)
           (a/close! failed-chan)))))


(cancel)

(take 1000 (filter filter-out-matched (shuffle (fs/glob (io/file "C:\\Users\\cross\\Desktop\\mt_o\\Artists\\") image-glob-pattern))))


(def last-mod (fs/last-modified-time "C:\\Users\\cross\\Desktop\\mt_o\\ArtistsNoEmb\\niliu_chahui\\1c1159645ffb56448bfb7cfea0ded6e7.json"))





(def expected-time  (jt/minus (jt/instant) (jt/minutes 45)))

(jt/after?  (.toInstant last-mod) expected-time)

(defn take-only-after [file]
  (let [last-mod (fs/last-modified-time file)]
    (jt/after?  (.toInstant last-mod) expected-time)))

(def files (fs/glob (io/file "C:\\Users\\cross\\Desktop\\mt_o\\ArtistsNoEmb\\") "**.json"))

(def filtered (filter take-only-after files))
(count filtered)

(doseq [file filtered]
  (fs/delete-if-exists file))
