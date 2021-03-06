(defproject cormac "0.1.0-SNAPSHOT"
  :description "Is it a code review tool? An art project? Both?"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.rrb-vector "0.0.11"]
                 [compojure "1.1.9"]
                 [ring/ring-core "1.3.1"]
                 [com.datomic/datomic-free "0.9.4899"]
                 [http-kit "2.1.19"]
                 [cheshire "5.3.1"]
                 [hiccup "1.0.5"]
                 [javax.servlet/servlet-api "2.5"]]
  :main cormac.core
  :aot [cormac.core])
