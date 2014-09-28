(ns cormac.http
  (:require [compojure.core :refer (GET POST defroutes context)]
    [compojure.handler :refer (api)]
    [ring.util.response :as r]
    [hiccup.core :as h]
    [hiccup.page :as p]
    [hiccup.form :as f]
    [hiccup.util :refer (escape-html)]
    [cormac.query :as q]
    [datomic.api :as d]
    [clojure.java.io :as io]
    [clojure.string :refer (blank?)]
    [clojure.java.shell :as shell])
  (:import java.util.UUID java.net.URL))

(def datomic-uri "datomic:free://localhost:4334/cormac")

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
          [:input {:type "text" :class "form-control" :name "uri" :id "uri" :placeholder "e.g. https://github.com/clojurecup2014/cormac.git"}]]
         [:button {:type "submit" :class "btn btn-primary btn-lg"} "Submit"]]]]
      [:div {:style "height: 20px;"}]
      [:div
       [:div [:p {:class "bg-info"} "Analyzed repos"]]
       [:div
        [:ul
         (let [db (:db req)
               data (q/qes '[:find ?e :where [?e :repo/uri]] db)]
           (for [i data :let [repo (first i)]]
             [:li [:a {:href (format "/repo/%s/" (str (:repo/id repo)))} (:repo/uri repo)]]))]]]])))

(defn repo-files [id req]
  (r/response
    (p/html5
      (p/include-css "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css")
      [:div {:class "container"}
       [:div [:p {:class "bg-info"} "Files"]]
       [:ul
        (let [db (:db req)
              data (q/qes '[:find ?e
                            :in $ ?id
                            :where [?p :repo/id ?id]
                                   [?p :repo/files ?e]] db (UUID/fromString id))]
          (for [i data :let [file (first i)]]
            [:li [:a {:href (format "/repo/%s/%s" (-> file :repo/_files :repo/id) (:file/path file))} (:file/path file)]]))]])))

(defn file-map [repo-id req]
  (let [file-path (get-in req [:params :*])
        content (line-seq  (io/reader (format "/var/tmp/repos/%s/%s" repo-id file-path)))]
    (r/response
     (p/html5
       (p/include-css "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css")
       [:div {:class "container"}
        [:div
         [:p {:class "bg-info"} file-path]]
        [:div
         (for [l content]
           [:pre {:style "border:0;margin:0;padding:0;"}
            (escape-html l)])]]))))

(defn invalid-url []
  (r/response "Please be a good citizen, use a valid Git repository url."))

(defn valid-url? [uri]
  (boolean
    (try
      (.toURI (URL. uri))
      (catch Exception _))))

(defn find-repo [db uri]
  (try
    (q/find-by db :repo/uri uri)
    (catch AssertionError _)))

(defn analyze [uri req]
  (if (valid-url? uri)
    (let [db (:db req)
          repo (find-repo db uri)]
      (if repo
        (let [pull (shell/with-sh-dir
                     (format "/var/tmp/repos/%s" (:repo/id repo)) (shell/sh "git" "pull"))]
          (if (zero? (:exit pull))
            (r/response "git pull succeed")
            (r/response "git pull failed")))
        (let [repo-id (d/squuid)
              tx-data [{:db/id (d/tempid :db.part/user)
                        :repo/uri uri
                        :repo/id repo-id}]
              result @(d/transact (:conn req) tx-data)
              clone (shell/with-sh-dir "/var/tmp/repos/" (shell/sh "git" "clone" uri (str repo-id)))]
          (if (zero? (:exit clone))
            (r/response "git clone succeed")
            (r/response "git pull failed")))))
    (invalid-url)))

(defroutes main-routes
  
  (GET "/" req
    (index req))
  
  (POST "/analyze" [uri :as req]
    (analyze uri req))

  (context "/repo/:id" [id]
    (GET "/" req
      (repo-files id req))
    (GET "/*" [id :as r]
      (file-map id r)))

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
  (-> main-routes
    (wrap-datomic datomic-uri)
    (api)))
