(ns cormac.heatmap
  (:require [clojure.string :as str]
            [hiccup.util :refer (escape-html)]))


(defn white->red [v]
  (let [n (Math/round (* 255 (- 1 v)))]
    [255 n n]))

(defn make-heatmap
  "file-heatmap is a seq of maps with :heat and :line key. Returns [:div [:pre ...] [:pre ...] ...]"
  [file-heatmap]
  (let [max-heat (apply max (map :heat file-heatmap))]
    [:div
     (for [l file-heatmap]
       [:pre {:style (format "border-radius:0; border:0;margin:0; padding:0; background-color: rgb(%s);"
                             (str/join "," (white->red (float (/ (dec (:heat l)) max-heat)))))}
        (escape-html (:line l))])]))

(defn build [file-content heatmap-vector]
  (let [file-heatmap (map (fn [line heat]
                            {:line line :heat heat})
                          (str/split file-content #"\n")
                          (concat heatmap-vector (repeat 1)))]
    (make-heatmap file-heatmap)))

(comment
  (cormac.heatmap/build "line1\nline2\nline3" [1 2 1]))
