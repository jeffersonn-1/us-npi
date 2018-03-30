(ns usnpi.npi
  (:require [usnpi.db :as db]
            [clojure.string :as str]
            [usnpi.http :as http]))

(def sql-limit 100)

;;
;; Helpers
;;

(defn- parse-ids
  [ids-str]
  (not-empty (re-seq #"\d+" ids-str)))

(defn- parse-words
  [term]
  (not-empty
   (as-> term $
     (str/split $ #"\s+")
     (remove empty? $))))

(defn- to-str
  [x]
  (if (keyword? x) (name x) (str x)))

(defn- as-bundle
  "Composes a Bundle JSON response from a list of JSON strings."
  [models]
  {:entry (map :resource models)})

(defn- gen-search-expression
  [fields]
  (->>
   fields
   (map (fn [[pr & pth]]
          (format "'%s:' || coalesce((resource#>>'{%s}'), '')"
                  (to-str pr) (str/join "," (mapv to-str pth)))))
   (str/join " || ' ' || \n")))

;;
;; Practitioner
;;

(def ^:private
  sql-like-clause-pract
  (gen-search-expression
   [[:g :name 0 :given 0]
    [:g :name 0 :given 1]
    [:p :name 0 :prefix 0]
    [:z :name 0 :siffix 0]
    [:f :name 0 :family]

    [:g :name 1 :given 0]
    [:g :name 1 :given 1]
    [:p :name 1 :prefix 0]
    [:z :name 1 :siffix 0]
    [:f :name 1 :family]

    [:s :address 0 :state]
    [:c :address 0 :city]]))

(defn sql-like-pract [term]
  (db/raw (format "\n%s ilike '%%%s%%'" sql-like-clause-pract term)))

(def trgrm_idx
  (format "CREATE INDEX IF NOT EXISTS pract_trgm_idx ON practitioner USING GIST ((\n%s\n) gist_trgm_ops);"
          sql-like-clause-pract))

;; drop
(def to-resource-expr "(resource || jsonb_build_object('id', id, 'resourceType', 'Practitioner'))")

(def query-practitioner
  {:select [:resource]
   :from [:practitioner]
   :where [:and [:not :deleted]]})

(defn get-practitioner
  [request]
  (let [npi (-> request :route-params :npi)
        q (update query-practitioner :where conj [:= :id npi])]
    (if-let [model (first (db/query (db/to-sql q)))]
      (http/json-resp (:resource model))
      (http/err-resp 404 "Practitioner with id = %s not found." npi))))

(defn get-practitioners-by-ids
  [request]
  (if-let [ids (some->> request :params :ids parse-ids)]
    (let [q (update query-practitioner :where conj [:in :id ids])
          models (db/query (db/to-sql q))]
      (http/json-resp (as-bundle models)))
    (http/err-resp 400 "Parameter ids is malformed.")))

(defn get-pracitioners
  [request]
  (let [words (some-> request :params :q parse-words)
        limit (-> request :params :_count (or sql-limit))

        q (assoc query-practitioner :limit limit)

        q (if-not (empty? words)
            (update q :where concat (map sql-like-pract words))
            q)]

    (http/json-resp (as-bundle (db/query (db/to-sql q))))))

;;
;; Organizations
;;

(def ^:private
  sql-like-clause-org
  (gen-search-expression
   [[:n :name]
    [:s :address 0 :state]
    [:c :address 0 :city]]))

(defn sql-like-org [term]
  (db/raw (format "\n%s ilike '%%%s%%'" sql-like-clause-org term)))

(def ^:private
  query-organization
  {:select [:resource]
   :from [:organizations]
   :where [:and [:not :deleted]]})

(defn get-organization
  "Returns a single organization entity by its id."
  [request]
  (let [npi (-> request :route-params :npi)
        q (update query-organization :where conj [:= :id npi])]
    (if-let [model (first (db/query (db/to-sql q)))]
      (http/json-resp (:resource model))
      (http/err-resp 404 "Organization with id = %s not found." npi))))

(defn get-organizations-by-ids
  "Returns multiple organization entities by their ids."
  [request]
  (if-let [ids (some->> request :params :ids parse-ids)]
    (let [q (update query-organization :where conj [:in :id ids])
          models (db/query (db/to-sql q))]
      (http/json-resp (as-bundle models)))
    (http/err-resp 400 "Parameter ids is malformed.")))

(defn get-organizations
  "Returns multiple organization entities by a query term."
  [request]
  (let [words (some-> request :params :q parse-words)
        limit (-> request :params :_count (or sql-limit))

        q (assoc query-organization :limit limit)

        q (if-not (empty? words)
            (update q :where concat (map sql-like-org words))
            q)]

    (http/json-resp (as-bundle (db/query (db/to-sql q))))))
