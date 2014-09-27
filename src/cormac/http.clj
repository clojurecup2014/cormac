(ns cormac.http
  (:require [compojure.core :refer (GET POST defroutes context)]
    [ring.util.response :as r]
    [hiccup.core :as h]
    [hiccup.page :as p]
    [hiccup.form :as f]
    [cormac.query :as q]
    [datomic.api :as d]))

(def datomic-uri "datomic:free://localhost:4334/cormac")

(comment
  
  [:ul
       (let [db (:db req)
             data (q/qes '[:find ?e :where [?e :repo/uri]] db)]
         (for [i data :let [repo (first i)]]
           [:li [:a {:href (:repo/uri repo)} (:repo/uri repo)]]))]
  
  )

(defn index [req]
  (r/response
    (p/html5
      [:head 
       [:title "Welcome to cormac"]]
      (p/include-css "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css")
      [:div {:class "container"}
       [:h1 "Welcome to cormac"]
      [:div
       [:div ""]
       [:div
        [:form {:method "post" :action "/analyze" :role "form"}
         [:div {:class "form-group"}
          [:label {:for "uri"} "Submit your repo - Repository URL"]
          [:input {:type "text" :class "form-control" :id "uri" :placeholder "e.g. https://github.com/clojurecup2014/cormac.git"}]]
         [:button {:type "submit" :class "btn btn-primary btn-lg"} "Heatmap"]]]]
      [:div {:style "height: 20px;"}]
      [:div
       [:div [:p {:class "bg-info"}"Analyzed repos"]]
       [:div
        [:ul
         (let [db (:db req)
               data (q/qes '[:find ?e :where [?e :repo/uri]] db)]
           (for [i data :let [repo (first i)]]
             [:li [:a {:href (:repo/uri repo)} (:repo/uri repo)]]))]]]])))

(defroutes main-routes
  
  (GET "/" req
    (index req))
  
  )


;; From https://gist.github.com/bobby/3150938

(defn wrap-datomic
  "A Ring middleware that provides a request-consistent database connection and
  value for the life of a request."
  [handler uri]
  (fn [request]
    (let [conn (d/connect uri)]
      (handler (assoc request
                 :conn conn
                 :db   (d/db conn))))))



(def app
  (wrap-datomic main-routes datomic-uri))
