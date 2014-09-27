(ns cormac.algo
  (:require [clojure.java.io :as io]
            [clojure.core.rrb-vector :as v]))

(defn cut [v idx len]
  (let [v1 (v/subvec v 0 idx)
        v2 (v/subvec v (+ idx len))]
    (v/catvec v1 v2)))

(defn splice [v1 idx v2]
  (v/catvec (v/subvec v1 0 idx)
            v2
            (v/subvec v1 idx)))

(defn map-part [v idx len f]
  (let [mv (v/vec (map inc (v/subvec v idx (+ idx len))))
        sv (cut v idx len)]
    (splice sv idx mv)))

(comment
  (cut [1 2 3 4 5 6 7 8] 3 2)
  ;; => [1 2 3 6 7 8]
  (splice [1 2 3 4 5 6] 2 [:a :b])
  ;; => [1 2 :a :b 3 4 5 6]
  (map-part [1 1 1 1 1 1 1 1] 3 2 inc)
  ;; => [1 1 1 2 2 1 1 1]
  )

(def repo-root "/var/tmp/repos")

(defn repo-uri [user repo]
  (str repo-root "/" user "/" repo))

(defn git-log-cmd [user repo]
  (let [uri (repo-uri user repo)]
    (format "git --git-dir=%s/.git --work-tree=%s log --reverse --no-merges -p --unified=0"
             uri
             uri)))

(defn sh [cmd]
  (-> (Runtime/getRuntime)
      (.exec cmd)
      .getInputStream
      io/reader
      line-seq))

(comment
  (take 10 (sh (git-log-cmd "clojure" "clojurescript"))))
