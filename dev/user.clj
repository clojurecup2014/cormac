(ns user
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer (pprint)]
            [datomic.api :as d]
            [cheshire.core :as json]
            [hiccup.core :as h]
            [hiccup.page :as p]
            [hiccup.util :refer (escape-html)]
            [cormac.algo :as algo]))

(comment

  (def datomic-uri "datomic:free://localhost:4334/cormac")

  (d/delete-database datomic-uri)
  (d/create-database datomic-uri)

  (def conn (d/connect datomic-uri))

  (def schema (read-string (slurp (io/resource "cormac/schema.edn"))))

  @(d/transact conn schema)

  ;; clojure repo sample, stored at /var/tmp/clojure/clojure



  (def sample-tx
    [{:db/id #db/id[:db.part/user -1]
      :repo/id (d/squuid)
      :repo/uri "https://github.com/clojure/clojure.git"
      :repo/files #db/id[:db.part/user -2]}

     {:db/id #db/id[:db.part/user -2]
      :file/path "antsetup.sh"
      :file/commit "03cd9d159a2c49a21d464102bb6d6061488b4ea2"
      :file/heatmap (json/generate-string [1 1 10 2 4 6 1 1])}])

  @(d/transact conn sample-tx)

  (def db (d/db conn))

  (def repo
    (d/q '[:find ?r :in $ ?uri
           :where [?r :repo/uri ?uri]] db "https://github.com/clojure/clojure.git"))

  (d/touch (d/entity db (ffirst repo)))

  (def file
    (d/q '[:find ?e
          :in $ ?r ?f
          :where [?p :repo/uri ?r]
                 [?p :repo/files ?e]
                 [?e :file/path ?f]] db "https://github.com/clojure/clojure.git" "antsetup.sh"))

  (def file2 (d/entity db (ffirst file)))

  (def heatmap (json/parse-string (:file/heatmap file2)))

  (def file-content (str/split (slurp (io/file "/var/tmp/repos/clojure/clojure/antsetup.sh")) #"\n"))

  (def file-heatmap (map (fn [s n] {:line s :heat n}) file-content heatmap))

  ;; from SO http://stackoverflow.com/a/18931093


  (defn color-temp [max-val min-val actual]
    (let [mid-val (/ (- max-val min-val) 2.0)]
      (if (>= actual mid-val)
        [255 (Math/round(* 255 (/ (- max-val actual) (- max-val mid-val)))) 0]
        [(Math/round (* 255 (/ (- actual min-val) (- mid-val min-val)))) 255 0])))

  (defn make-heatmap [file-heatmap]
    (spit (io/file "/tmp/test.html")
    (p/html5
     [:div
      (for [l file-heatmap]
        [:pre {:style (format "margin:0; padding:0; background-color: rgb(%s);"
                              (str/join "," (color-temp 10 1 (:heat l))))}
         (escape-html (:line l))])])))

  (let [heat-vector (get  (algo/build-heat-vectors  (algo/parse-log "clojure" "clojurescript")) "src/cljs/cljs/reader.cljs")
        lines (line-seq  (io/reader "/var/tmp/repos/clojure/clojurescript/src/cljs/cljs/reader.cljs"))]
    (make-heatmap (map (fn [s n] {:line s :heat n}) lines heat-vector))))
