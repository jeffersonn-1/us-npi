(ns usnpi.search-test
  (:require [clojure.test :refer :all]
            [usnpi.util-test :refer [read-body]]
            [ring.mock.request :as mock]
            [usnpi.core :as usnpi]))

(defn benchmark [f]
  (time (f)))

(use-fixtures
  :each
  benchmark)

(def url "/search")

; Playing life on hard mode
; We test search with a selection of orgs that were particularly hard to find
; See https://github.com/eblechman/RK360Backend/issues/343

(def freehand-names [
   "Health Services of North Texas"
   "Dr. Siegel"
   "THR Texas Health Presbyterian Hospital Denton"
   "Medical City of Denton"
   ])

(def npi-names [{:name "HEALTH SERVICES OF NORTH TEXAS, INC"}
                {:first_name "JASON" :last_name "SIEGEL"}
                {:name "TEXAS HEALTH PRESBYTERIAN HOSPITAL DENTON"}
                {:name "COLUMBIA MEDICAL CENTER OF DENTON SUBSIDIARY LP"}
                ])

(defn find-organization [org]
  (testing (str "Trying to find " org)
    (let [res (usnpi/app (mock/request :get url (assoc org :count 1)))]
      (is (= (:status res) 200))
      (println (-> res read-body :entry first))
      (is (-> res read-body :entry count (not= 0))))))

(deftest test-search-api
  (mapv find-organization npi-names))
      

(deftest test-approx-search
  ;FIXME
  )