(ns user
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer (pprint)]
            [datomic.api :as d]
            [cheshire.core :as json]))
(comment
  
  (def datomic-uri "datomic:free://localhost:4334/cormac")
  
  (d/create-database datomic-uri)
  
  (def conn (d/connect datomic-uri))
  
  (def schema (read-string (slurp (io/resource "cormac/schema.edn"))))
  
  @(d/transact conn schema)
  
  ;; clojure repo sample, stored at /var/tmp/clojure/clojure
  
  
  
  (def sample-tx
    [{:db/id #db/id[:db.part/user -1]
      :repo/uri "https://github.com/clojure/clojure.git"
      :repo/files #db/id[:db.part/user -2]}
     
     {:db/id #db/id[:db.part/user -2]
      :file/path "antsetup.sh"
      :file/commit "03cd9d159a2c49a21d464102bb6d6061488b4ea2"
      :file/heatmap (json/generate-string [1 1 10 2 4 6 1 1])}])
  
  @(d/transact conn sample-tx)

  (def db (d/db conn))

  (def file
    (d/q '[:find ?e
          :in $ ?r ?f
          :where [?p :repo/uri ?r]
                 [?p :repo/files ?e]
                 [?e :file/path ?f]] db "https://github.com/clojure/clojure.git" "antsetup.sh"))

  (def file2 (d/entity db (ffirst file)))

  (prn (:file/heatmap file2))

  )