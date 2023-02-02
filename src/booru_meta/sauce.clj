(ns booru-meta.sauce
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [typed.clojure :as t]
            [clj-http.client :as client]
            [clojure.string :as s]
            [hickory.select :as hs]
            [hickory.core :as html]
            [bites.core])
  (:import (java.nio.file FileSystems)
           org.apache.commons.codec.binary.Hex)
  (:use [booru-meta.utils]
        [booru-meta.common]))


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
        ua (if-let [u (:user-agent options)] u default-user-agent)
        three-d? (if (some? (:3d? options)) (:3d? options) false)
        url (if three-d? "https://3d.iqdb.org/" "https://iqdb.org/")
        source :iqdb
        ret (promise)]
    (client/post url {:async true :multipart [{:name "file" :content body}] :as :auto :headers {"User-Agent" ua}}
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
        ua (if-let [u (:user-agent options)] u default-user-agent)
        url "https://ascii2d.obfs.dev/"
        append-url (fn [url path] (let [uri (java.net.URI/create url)]
                                    (str (.resolve uri path))))
        is-bovw (if (some? (:bovw? options)) (:bovw? options) false)
        source :ascii2d
        ret (promise)]
    (client/post (append-url url "/search/file") {:async true :multipart [{:name "file" :content body}] :as :auto :headers {"User-Agent" ua}}
                 (fn [response]
                   (let [new-url (get-in response [:headers "Location"])
                         new-url (if is-bovw (s/replace-first new-url #"\/color\/" "/bovw/") new-url)]
                     (if (some? new-url)
                       (client/get new-url {:async true :as :auto :headers {"User-Agent" ua}}
                                   (fn [response]
                                     (let [data (extract-ascii2d-info (:body response))]
                                       (if (some? data) (deliver ret {:data data :source source})
                                           (deliver ret {:error :no-match :source source}))))
                                   (fn [error] (deliver ret {:error error :source source})))
                       (deliver ret {:error :no-match :source source}))))
                 (fn [error] (deliver ret {:error error :source source})))
    ret))
