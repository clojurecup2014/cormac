(ns cormac.http
  (:require [cormac.algo :refer (build-tx)]
    [cormac.heatmap :as heatmap :only (build)]
    [compojure.core :refer (GET POST defroutes context)]
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
    [clojure.java.shell :as shell]
    [cheshire.core :as json :only (parse-string)])
  (:import java.util.UUID java.net.URL))

(def datomic-uri "datomic:free://localhost:4334/cormac")

(defn index [req]
  (r/response
    (p/html5
      [:head
       [:title "Welcome to cormac"]]
      (p/include-css "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css")
      (p/include-css "/public/cormac.css")
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
       [:div {:style "background: #fff;"}
        [:p {:class "bg-info"} "Analyzed repos"]
        [:ul
         (let [db (:db req)
               data (q/qes '[:find ?e :where [?e :repo/uri]] db)]
           (for [i data :let [repo (first i)] :when (:repo/files repo)]
             [:li [:a {:href (format "/repo/%s/" (str (:repo/id repo)))} (:repo/uri repo)]]))]]]])))

(defn repo-files [id req]
  (r/response
    (p/html5
      (p/include-css "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css")
      (p/include-css "/public/cormac.css")
      [:div {:class "container"}
       [:p {:class "bg-info"} "Files"]
       [:ul {:style "background: #fff;"}
        (let [db (:db req)
              data (q/qes '[:find ?e
                            :in $ ?id
                            :where [?p :repo/id ?id]
                                   [?p :repo/files ?e]] db (UUID/fromString id))]
          (let [entities (->> (map first data)
                              (sort-by :file.heatmap/avg)
                              reverse)]
            (for [file entities
                  :let [path (.substring (:file/path file) 37)]]
              [:li [:a {:href (format "/repo/%s/%s" (-> file :repo/_files :repo/id) path)} path]])))]])))

(defn heatmap [db file-path]
  (try
    (let [f (q/find-by db :file/path file-path)]
      (json/parse-string (:file/heatmap f)))
    (catch AssertionError _ [])))

(defn file-map [repo-id req]
  (let [file-path (get-in req [:params :*])
        content (slurp (io/reader (format "/var/tmp/repos/%s/%s" repo-id file-path)))
        hm (heatmap (:db req) (format "%s/%s" repo-id file-path))]
    (r/response
     (p/html5
       (p/include-css "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css")
       (p/include-css "/public/cormac.css")
       [:div {:class "container"}
        [:div
         [:p {:class "bg-info"} file-path]]
        [:div
         (heatmap/build content hm)]]))))

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
        (do
          (future
            (let [pull (shell/with-sh-dir
                         (format "/var/tmp/repos/%s" (:repo/id repo)) (shell/sh "git" "pull"))
                  tx-data (if (zero? (:exit pull))
                            (build-tx (:repo/id repo) (:db/id repo)))]
              (if tx-data
                @(d/transact (:conn req) tx-data))))
          (r/response "We're pulling the latest version. Go back to the front page, wait patiently, and refresh!"))
        (do
          (future
            (let [repo-id (d/squuid)
                  tempid (d/tempid :db.part/user)
                  tx-data [{:db/id tempid
                            :repo/uri uri
                            :repo/id repo-id}]
                  result @(d/transact (:conn req) tx-data)
                  repo-db-id (d/resolve-tempid (:db-after result) (:tempids result) tempid)
                  clone (shell/with-sh-dir "/var/tmp/repos/" (shell/sh "git" "clone" uri (str repo-id)))]
              (if (zero? (:exit clone))
                (let [tx-data (build-tx repo-id repo-db-id)]
                  (if tx-data
                    (do @(d/transact (:conn req) tx-data)
                        (r/response "git clone succeed"))))
                (r/response "git pull failed"))))
          (r/response "Hey, new repo, thanks! Go back to the front page, wait patiently, and refresh!"))))
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
