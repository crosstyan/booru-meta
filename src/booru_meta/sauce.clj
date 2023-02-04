(ns booru-meta.sauce
  "sauce API
  Every function should return
  
  ```clojure
  {;; nil when error
   :data map | nil
   ;; nil when no error
   :error keyword | exception | nil 
   ;; most of time it's nil except for sauceNAO. It's the
   ;; extra info that sauceNAO returns in header (like API usage)
   :extra map | nil 
   :soruce keyword
   }
  ```
   "
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [typed.clojure :as t]
            [clj-http.client :as client]
            [clojure.string :as s]
            [hickory.select :as hs]
            [hickory.core :as html]
            [bites.core]
            [malli.core :as m]
            [booru-meta.schema :as schema])
  (:import (java.nio.file FileSystems)
           org.apache.commons.codec.binary.Hex)
  (:use [booru-meta.utils]
        [booru-meta.common]))


;; https://github.com/kitUIN/PicImageSearch/blob/3c7f13cab5d49d38f6236f8f26f54a2e64d06175/PicImageSearch/saucenao.py#L27
(defn sauce [file options]
  (let [body (read-compress-img file)
        min-sim (get options :min-sim 0.8)
        api-key (:api-key options)
        to-final' (fn [result]
                    {:similarity (/ (Float/parseFloat (get-in result [:header :similarity])) 100)
                     :link (get-in result [:data :ext_urls 0])
                     :author {:name (first (filter some?
                                                   [(get-in result [:data :member_login_name])
                                                    (get-in result [:data :creator])]))}
                     :meta (:data result)})
        to-final #(map to-final' %)
        to-extra (fn [header]
                   {:short-remaining (:short_remaining header)
                    :long-remaining (:long_remaining header)
                    :short-limit (:short_limit header)
                    :long-limit (:long_limit header)})
        ret (promise)
        url "https://saucenao.com/search.php"
        source :saucenao
        params {:output_type 2
                :api_key api-key
                :testmode 0}]
    (assert (and (> min-sim 0) (< min-sim 1)) "min-sim should be between 0 and 1")
    (if (some? api-key)
      (client/post url {:async true :multipart [{:name "file" :content body}] :query-params params :as :json}
                   (fn [response]
                     (deliver ret {:data {:final (filter #(>= (:similarity %) min-sim) (to-final (get-in response [:body :results])))} 
                                   :extra (to-extra (get-in response [:body :header])) 
                                   :source source}))
                   (fn [error] (deliver ret {:error error :source source})))
      (deliver ret {:error :no-api-key}))
    ret))

;; iqdb will return a html page. Need to parse it.
;; Hickory Tips: you can't match a list. Have to use `map` or `first`.
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

(defn iqdb
  ([file] (iqdb file {}))
  ([file options]
   (let [body (read-compress-img file)
         ua (if-let [u (:user-agent options)] u default-user-agent)
         three-d? (if-let [f (:3d? options)] f false)
         to-final (fn [d] {:similarity (:sim d)
                           :link (:link d)
                           :meta (:meta d)})
         min-sim (if-let [s (:max-sim options)] s 0.8)
         min-sim (if-let [s (#(if (and (> % 0) (< % 1)) % nil) min-sim)] s 0.8)
         url (if three-d? "https://3d.iqdb.org/" "https://iqdb.org/")
         source :iqdb
         ret (promise)]
     (client/post url {:async true :multipart [{:name "file" :content body}] :as :auto :headers {"User-Agent" ua}}
                  (fn [response]
                    (let [data (extract-iqdb-info (:body response))
                          data (filter #(>= (:sim %) min-sim) data)]
                      (deliver ret (if (some? data) {:data {:final (map to-final data)} :source source} {:error :no-match}))))
                  (fn [error] (deliver ret {:error error :source source})))
     ret)))


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

(defn ascii2d
  "Get the result from ascii2d.net.
 Personally I don't recommend using this site 
 since it only return the result without similarity."
  ([file] (ascii2d file {}))
  ([file options]
   (let [body (read-compress-img file)
         ua (if-let [u (:user-agent options)] u default-user-agent)
         url "https://ascii2d.obfs.dev/"
         append-url (fn [url path] (let [uri (java.net.URI/create url)]
                                     (str (.resolve uri path))))
         is-bovw (if-let [b (:bovw? options)] b false)
         to-final (fn [d] {:link (get-in d [:work :link]) 
                           :author (:author d)})
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
                                        (if (some? data) (deliver ret {:data {:final (map to-final data)} :source source})
                                            (deliver ret {:error :no-match :source source}))))
                                    (fn [error] (deliver ret {:error error :source source})))
                        (deliver ret {:error :no-match :source source}))))
                  (fn [error] (deliver ret {:error error :source source})))
     ret)))
