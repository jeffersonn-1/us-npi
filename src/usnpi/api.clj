(ns usnpi.api
  "REST API handlers."
  (:require [usnpi.db :as db]
            [usnpi.beat :as beat]
            [usnpi.env :refer [env]]
            [usnpi.warmup :as wm]
            [cheshire.core :as json]))

(defn- json-resp
  [data]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string data {:pretty true})})

(defn api-env
  "An API that returns some of ENV vars."
  [request]
  (json-resp
   (select-keys env [:git-commit])))

(defn api-updates
  "Returns the latest NPI updates."
  [request]
  (json-resp
   (db/query "select * from npi_updates order by date desc limit 100")))

(defn api-tasks
  "Returns the list of regular tasks."
  [request]
  (json-resp
   (db/query "select * from tasks order by id")))

(defn api-beat
  "Returns the status of the beat subsystem."
  [request]
  (json-resp
   {:status (beat/status)}))

(defn api-pg-state
  "Returns Postgres cache statistics: how many cache blocks
  are loaded at the moment for each relation. A relation might be
  not only a table but also an index, a view, etc."
  [request]
  (json-resp
   {:cache (wm/cache-stats)
    :settings (wm/db-settings)}))

;;
;; Ops
;;

(defn api-reset-tasks
  "Resets the DB data to make all the tasks to be run immediately."
  [request]
  (db/with-tx
    (db/execute! "truncate npi_updates")
    (db/execute! "update tasks set next_run_at = (current_timestamp at time zone 'UTC') + random() * interval '300 seconds'" ))
  (when-not (beat/status)
    (beat/start))
  (json-resp
   {:status true}))

(defn api-trigger-full-import
  "Drops the data and runs full import."
  [request]
  (db/with-tx
    (db/execute! "truncate practitioner")
    (db/execute! "truncate organizations")
    (db/execute! "update tasks set next_run_at = now() where handler = 'usnpi.update/task-full-dissemination'")
    (db/execute! "truncate npi_updates"))
  (when-not (beat/status)
    (beat/start))
  (json-resp
   {:status true}))

(defn api-pg-warmup-index
  "Warms index cache blocks on demand."
  [request]
  (json-resp
   (wm/task-warmup-index)))

(defn api-logs
  "Returns a log data."
  [request]
  {:status 200
   :body (slurp "usnpi.log")})
