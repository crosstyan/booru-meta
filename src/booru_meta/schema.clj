(ns booru-meta.schema
  (:require [malli.core :as m]
            [malli.util :as mu]))

(def error
  (m/schema
   [:map
    [:error :keyword]
    [:data {:optional true} :nil]
    [:source [:or :keyword :string]]]))

(def sucess
  (m/schema
   [:map
    [:data :map]
    [:error {:optional true} :nil]
    [:source [:or :keyword :string]]
    [:extra {:optional true} :map]]))

(def result
  "Schema for the result from `sauce` or `booru`.
 In theoy `:data` and :error should be present together."
  (m/schema
   [:or sucess error]))

(def booru-final
  "Schema for the `:final` field in the result from `booru`."
  (m/schema [:map
             [:id [:or :int :string]]
             [:rating :string] ;; g/s/e general/sensitive/explicit (or safe?)
             [:md5 :string]
             [:general [:sequential :string]]
             [:artist [:sequential :string]]
             [:copyright [:sequential :string]]
             [:character [:sequential :string]]
             [:meta [:sequential :string]]]))

(def booru-data-schema
  "Schema for the `:data` field in the result from `booru`."
  (m/schema [:map
             [:original {:optional true} :map]
             [:final booru-final]]))

(def booru-result
  "Schema for the result from `booru`."
  (let [sucess
        (m/walk sucess
                (m/schema-walker
                 #(mu/update-properties % assoc :data booru-data-schema)))]
    [:or sucess error]))


(def sauce-final
  (let [final
        (m/schema [:map
                   [:link :string]
                   ;; for sauceNAO
                   [:links {:optional true} [:sequential :string]]
                   [:similarity {:min 0 :max 1} float?]
                   [:meta {:optional true} :map]
                   [:author {:optional true}
                    [:map
                     [:name :string]
                     [:link {:optional true} :string]]]])]
    (m/schema [:sequential final])))


(def sauce-result
  (let [sauce (m/schema
               [:map
                [:original {:optional true} :map]
                [:final sauce-final]])
        sucess (m/walk sucess
                       (m/schema-walker
                        #(mu/update-properties % assoc :data sauce)))]
    (m/schema [:or sucess error])))

(def path
  (m/schema [:map
             [:absolute :string]
             [:relative {:optional true} :string]]))

(def persistent-info
  "schema for v0.1"
  (m/schema
   [:map
    [:data [:map-of :keyword result]]
    [:path path]
    [:embedding {:optional true} :string]
    [:md5 :string]
    [:version :string]]))

(def no-match
  (m/schema
   [:map-of :keyword :boolean]))
