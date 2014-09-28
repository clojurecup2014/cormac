(ns cormac.core
  (:require [cormac.http :as http]
    [clojure.java.io :as io :only (resource)]
    [org.httpkit.server :refer (run-server)]
    [datomic.api :as d])
  (:gen-class))

(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn -main []
  (let [uri "datomic:free://localhost:4334/cormac"
        schema (read-string (slurp (io/resource "cormac/schema.edn")))]
    (d/create-database uri)
    @(d/transact (d/connect uri) schema)
    (reset! server (run-server #'http/app {:port 8080}))))

;; (-main)
