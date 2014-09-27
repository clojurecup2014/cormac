(ns cormac.algo
  (:require [clojure.core.rrb-vector :as v]))

(defn cut [v idx len]
  (let [v1 (v/subvec v 0 idx)
        v2 (v/subvec v (+ idx len))]
    (v/catvec v1 v2)))

(comment
   (cut [1 2 3 4 5 6 7 8] 3 2)
   ;; => [1 2 3 6 7 8]
   )

(defn splice [v1 idx v2]
  (v/catvec (v/subvec v1 0 idx)
            v2
            (v/subvec v1 idx)))

(comment
   (splice [1 2 3 4 5 6] 2 [:a :b])
   ;; => [1 2 :a :b 3 4 5 6]
   )

(defn map-part [v idx len f]
  (let [mv (v/vec (map inc (v/subvec v idx (+ idx len))))
        sv (cut v idx len)]
    (splice sv idx mv)))

(comment
   (map-part [1 1 1 1 1 1 1 1] 3 2 inc)
   ;; => [1 1 1 2 2 1 1 1]
   )
