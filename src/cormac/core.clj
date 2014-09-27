(ns cormac.core
  (:require [cormac.http :as http]
    [org.httpkit.server :refer (run-server)]))

(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn -main []
  (reset! server (run-server #'http/app {:port 8080})))
