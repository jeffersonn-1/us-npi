(ns usnpi.search
  (:require [usnpi.http :as http]
            [usnpi.db :as db]
            [usnpi.npi :as npi]
            [honeysql.core :as sql]
            [honeysql.helpers :refer :all :as helpers]
            [honeysql.format :as sqlf]
            [clojure.string :as str]
            [clojure.tools.logging :as log]))

(defmacro resource [& fields]
  (let [s (str "resource#>>'{"
               (str/join "," (map (fn [field]
                                    (if (keyword field)
                                      (name field)
                                      field))
                                  fields))
               "}'")]
    `(db/raw ~s)))

(defn build-where [where {:keys [postal-codes state city]}]
  (cond-> where
    city         (conj [:ilike (resource :address 0 :city) (str "%" city "%")])
    state        (conj [:=     (resource :address 0 :state) state])
    postal-codes (conj [:in    (resource :address 0 :postalCode) postal-codes])))

(defn only-organization? [{:keys [name org first-name last-name]}]
  (and org (not name) (not first-name) (not last-name)))

(defn only-practitioner? [{:keys [name org first-name last-name]}]
  (and (or first-name last-name) (not name) (not org)))

(defn with-count [query {:keys [count]}]
  (cond-> query
    count (assoc :limit count)))

(defn with-type [query params pred type]
  (if (pred params)
    query
    (update query :select conj [type :type])))


;;General

(defn state-check [state]
  (db/raw (str "resource @?? '$.address[*].state ? (@ starts with \"" (str/upper-case state) "\")'::jsonpath")))

(defn city-check [city]
  (db/raw (str "resource @?? '$.address[*].city[*] ? (@ starts with \"" (str/upper-case city) "\")'::jsonpath")))

(defn postal-code-check [postal-code]
  (db/raw (str "resource @?? '$.address[*].postalCode[*] ? (@ starts with \"" (str postal-code)  "\")'::jsonpath")))



(defn taxonomy-check-practitioner [taxonomies]
  (db/raw (str "resource @?? '$.qualification[*].code.coding[*].code ? (" (str/join " || " (for [t taxonomies] (str "@ == \"" t "\""))) ")'::jsonpath")))

(defn first-name-check [first-name]
  (db/raw (str "resource @?? '$.name[*].given[*] ? (@ starts with \"" (str/upper-case first-name) "\")'::jsonpath")))

(defn family-name-check [family-name]
  (db/raw (str "resource @?? '$.name[*].family ? (@ starts with \"" (str/upper-case family-name) "\")'::jsonpath")))

;;Org

(defn taxonomy-check-organization [taxonomies]
  (db/raw (str "resource @?? '$.type[*].coding.code[*] ? (" (str/join " || " (for [t taxonomies] (str "@ == \"" t "\""))) ")'::jsonpath")))

(defn name-check-organization [name]
  (db/raw (str "resource @?? '$.name[*] ? (@ starts with \"" (str/upper-case name) "\")'::jsonpath")))

(defn build-practitioner-sql [{:keys [name first-name last-name taxonomies] :as params}]
  (when-not (only-organization? params)
    (let [family     (or name last-name)
          first-name (and (not name) first-name)
          name-col   (resource :name 0 :family)]
      (-> {:select [:id :resource [name-col :name]]
           :from [:practitioner]
           :where (cond-> [:and [:= :deleted false]]
                   ;; family       (conj (family-name-check family))
                    ;;first-name   (conj (first-name-check first-name))
                    taxonomies   (conj (taxonomy-check-practitioner taxonomies))
                    :always      (build-where params))}
          (with-count params)
          (with-type params only-practitioner? 1)))))

(defn build-organization-sql [{:keys [name org count taxonomies] :as params}]
  (when-not (only-practitioner? params)
    (let [org (or name org)
          name-col (resource :name)]
      (-> {:select [:id :resource [name-col :name]]
           :from [:organizations]
           :where (cond-> [:and [:= :deleted false]]
                    ;;org          (conj [:ilike name-col (str "%" org "%")])
                    taxonomies   (conj (taxonomy-check-organization taxonomies))
                    :always      (build-where params))}
          (with-count params)
          (with-type params only-organization? 2)))))

(defn wrap-query [query]
  {:select [:id :resource]
   :from [[query :q]]
   :order-by [:name]})

(defmethod sqlf/format-clause :union-practitioner-and-organization [[_ [left right]] _]
  (str "(" (sqlf/to-sql left) ") union all (" (sqlf/to-sql right) ")"))

(defn union [practitioner-sql organization-sql]
  {:select [:id :resource]
   :from [[{:union-practitioner-and-organization [practitioner-sql
                                                 organization-sql]} :q]]
   :order-by [:type :name]})

(defn as-vector [s]
  (when-not (str/blank? s)
    (str/split s #",")))

(defn parse-int [number-string]
  (try (Integer/parseInt number-string)
       (catch Exception e nil)))

(defn normalize-count [& count]
  (or (parse-int count) 50))

(def taxanomy-code
  {:emergency_medicine "207P00000X"
   :ambulance "341600000X"
   :ambulance_land_transport "3416L0300X"
   :ambulance_air_transport "3416A0800X"
   :clinic_center_adult_day_care "261QA0600X"
   :internal_medicine_gastroenterology "207RG0100X"
   :hospitalist "208M00000X"
   :pharmacy_community_retail "3336C0003X"})


(defn search-t-codes [taxanomy]
  (if (empty? taxanomy)
    nil
    (let [taxonomies (filter #(re-matches  (re-pattern (str taxanomy".*" )) %)
                             (map name (keys taxanomy-code)))
          select-values (comp vals select-keys)
          t-codes (select-values taxanomy-code (map keyword taxonomies))]
      t-codes)))


(defn query-practitioner [{:keys [first-name
                                  last-name
                                  postal-code
                                  state
                                  city
                                  taxonomies
                                  speciality] :as params}]  
  (sql/format (->
               (select :id :resource)
               (from :practitioner)
               (where (cond-> [:and [:= :deleted false]]
                        speciality (conj (taxonomy-check-practitioner speciality))
                        taxonomies (conj (taxonomy-check-practitioner taxonomies))
                        first-name (conj (first-name-check first-name))
                        last-name (conj (family-name-check last-name))
                        state (conj (state-check state))
                        city  (conj (city-check city))
                        postal-code  (conj (postal-code-check postal-code))))
               (limit (normalize-count)))))

(defn query-organizations [{:keys [org
                                   state
                                   city
                                   postal-code
                                   taxonomies
                                   speciality] :as params}]
  (sql/format (->
               (select :id :resource)
               (from :organizations)
               (where (cond-> [:and [:= :deleted false]]
                         speciality (conj (taxonomy-check-organization speciality))
                         taxonomies (conj (taxonomy-check-organization taxonomies))
                         org (conj (name-check-organization org))
                         state (conj (state-check state))
                         city  (conj (city-check city))
                         postal-code  (conj (postal-code-check postal-code))))
               (limit (normalize-count)))))




 
(defn wrap-query-organizations [params]
  (if (seq params)
    (db/query (query-organizations params))
    nil ))

(defn wrap-query-practitioner [params]
  (if (seq params)
    (db/query (query-practitioner params))
    nil ))



;;(search-t-codes "hospital")

(defn check-condition [data]
  (log/info data)
  (if (empty? data)
    (str "Error: search requires atleast one parameter.")
    (let [params (assoc data
                        :speciality (search-t-codes (:speciality data))
                        :taxonomies (if (empty? (:taxonomies data))
                                      nil
                                      (if (string? (:taxonomies data))
                                        (list (:taxonomies data))
                                        (apply list (:taxonomies data))
                                        )))
          ]
        (log/info params)
        (if (and (not (empty? (select-keys params [:first-name :last-name])) )
                 (contains? params :org))
          (str "Error: You searched for the names of a person and an organization at the same time. Choose just one.")
          (cond 
            (or (contains?  params :first-name)
                (contains?  params :last-name)) (wrap-query-practitioner params)
            (contains?  params :org) (wrap-query-organizations params)
            :else  
             (merge (wrap-query-practitioner params)
                    (wrap-query-organizations params))
            )))
      )
  )






(defn search [{params :params}]
  (http/http-resp (npi/as-bundle (check-condition params))))



;;(check-condition {:state "tx"})

;; -> How search works
;; - `GET /search`

;; - "first-name:Leo  searches practitioners where `resource#>>'{name,0,given}'` includes `first-name`."
;; - "last-name:Brodie searches practitioners where `resources#>>'{name,0,family}'` starts with `last-name`.\"
;; - "`org` searches organizations where `resource#>>'{name}`"
;; - "(or First-name  last-name) and organization can not be searched together"
;; - "`speciality:emergency` searches both organizations and practitioners who are provind emergency service. It filters by using taxanomy codes and get results"
;; - `postal-code:80304 ` searches practitioners or organizations where `resource#>>'{address,0,postalCode}'` in specified `postal-codes`.
;; - `state:AZ` searches practitioners or organizations where `resource#>>'{address,0,state}'` equal specified `state`.
;; - `city:New York` searches practitioners or organizations where `resource#>>'{address,0,city}'` includes specified `city`.
;; - `count:50` limit returned results.

;; If changes required pls suggest me


