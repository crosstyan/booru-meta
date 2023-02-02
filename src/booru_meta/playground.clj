(ns booru-meta.playground
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [typed.clojure :as t]
            [bites.core]
            [booru-meta.sauce :as sauce])
  (:import (java.nio.file FileSystems)
           org.apache.commons.codec.binary.Hex)
  (:use [booru-meta.utils]
        [booru-meta.sauce]
        [booru-meta.booru]
        [booru-meta.core]))

;; (s/join ", " (map #(format "0x%02x" %) (hex->bytes (s/replace  long-str #"\s+" ""))))
(def long-list [2, 128, 2, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31,
                31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 15, 1, 1, 1,
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                13, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 1, 1, 12, 6, 6, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
                4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 44, 4, 255, 255, 255, 255, 255, 255, 255, 255,
                255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                255, 255, 255, 255, 255, 255, 255, 255])

(count long-list)

(def files (glob "/Volumes/Untitled 1/Grabber" image-glob-pattern))
(categorize-by-md5 files)

(def cancel (run-batch (shuffle (:md5 (categorize-by-md5 files))) {:max-limit 20 :reset-interval-ms 25000 :root-path "/Volumes/Untitled 1/Grabber" :random-delay-ms [100 400]}))
(cancel)

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
@(danbooru "0c0d79e53b5e50c568baea759194476f")

;; pay attention to short_remaining and long_remaining
;; if short_remaining is 0, wait for 30 seconds
(sauce (io/file "/Volumes/Untitled 1/Grabber/maachi/90375559_p0 - 無題.jpg") {:api-key "009934e06a88a3a1f28c565d69a5273ee47008e1"})

;; https://github.com/clj-commons/hickory
(iqdb (io/file "/Volumes/Untitled 1/Grabber/kazuharu_kina/33767cc3b60dcebb3733854dd03b7da5.jpg") {:3d? false})
(ascii2d (io/file "/Volumes/Untitled 1/Grabber/kazuharu_kina/33767cc3b60dcebb3733854dd03b7da5.jpg") {})

;; https://stackoverflow.com/questions/10062967/clojures-equivalent-to-pythons-encodehex-and-decodehex

((comp bytes->string calc-md5) (io/file "/Volumes/Untitled 1/Grabber/kazuharu_kina/33767cc3b60dcebb3733854dd03b7da5.jpg"))
;; (-> (io/file "/Volumes/Untitled 1/Grabber/kazuharu_kina/33767cc3b60dcebb3733854dd03b7da5.jpg")
;;     (calc-md5)
;;     (bytes->string))
