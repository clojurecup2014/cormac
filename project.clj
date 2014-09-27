(defproject cormac "0.1.0-SNAPSHOT"
  :description "Is it a code review tool? An art project? Both?"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.rrb-vector "0.0.11"]
                 [compojure "1.1.9"]
                 [ring "1.3.1"] ;; TODO: change to only required libs
                 [com.datomic/datomic-free "0.9.4899"]])
