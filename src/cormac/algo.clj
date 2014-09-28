(ns cormac.algo
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.core.rrb-vector :as v]
            [cheshire.core :as json]))

(defn cut [v idx len]
;;  (println idx len)
  (let [v1 (v/subvec v 0 idx)
        v2 (if (< (count v) ;; TODO!!!
                  (+ idx len))
             (v/vector)
             (v/subvec v (+ idx len)))]
    (v/catvec v1 v2)))

;;(count (cut (vec-of 801 1) 758 44))

(defn splice [v1 idx v2]
  (v/catvec (into [] (v/subvec v1 0 idx))
            (into [] v2)
            (into [] (v/subvec v1 idx))))

(defn map-part [v idx len f]
;;  (println v idx len)
  (let [mv (v/vec (map inc (v/subvec v idx (+ idx len))))
        sv (cut v idx len)]
    (splice sv idx mv)))

(defn vec-of [length fill]
  (v/vec (repeat length fill)))



(comment
  (cut [1 2 3 4 5 6 7 8] 3 2)
  ;; => [1 2 3 6 7 8]
  (splice [1 2 3 4 5 6] 2 [:a :b])
  ;; => [1 2 :a :b 3 4 5 6]
  (splice [1 2 3] 3 [4 5])

  (map-part [1 1 1 1 1 1 1 1] 3 2 inc)
  ;; => [1 1 1 2 2 1 1 1]
  (vec-of 10 2)
  ;; => [2 2 2 2 2 2 2 2 2 2]
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
  (nth (parse-log "clojure" "clojurescript")
       1207))

(defn resolve-overlap [{{delete-start :start delete-length :length} :delete
                        {insert-start :start insert-length :length} :insert
                        :as hunk}]
  (cond
   (or (nil? (:delete hunk))
       (nil? (:insert hunk)))
   (into [] hunk)

   ;; ++++
   ;; ----
   (= (:delete hunk) (:insert hunk))
   [[:edit (:delete hunk)]]

   ;; +++
   ;; ------
   ;; or
   ;; +++++
   ;; ---
   (= delete-start insert-start)
   (if (< delete-length insert-length)
     [[:edit {:start delete-start
              :length delete-length}]
      [:insert {:start (+ delete-start delete-length)
                :length (- insert-length delete-length)}]]
     [[:edit {:start insert-start
              :length insert-length}]
      [:delete {:start (+ insert-start insert-length)
                :length (- delete-length insert-length)}]])

   ;;   ++++++
   ;; -----
   (< delete-start insert-start (+ delete-start delete-length) (+ insert-start insert-length))
   (let [new-delete-length (- insert-start delete-start)
         new-insert-start (+ delete-start delete-length)
         edit-start (+ delete-start new-delete-length)
         new-insert-length (- insert-length (- new-insert-start insert-start))
         edit-length (- new-insert-start edit-start)]
     [[:delete {:start delete-start
                :length new-delete-length}]
      [:insert {:start new-insert-start
                :length new-insert-length}]
      [:edit {:start edit-start
              :length edit-length}]])

   ;; +++++
   ;;   -----
   (< insert-start delete-start (+ insert-start insert-length) (+ delete-start delete-length))
   (let [edit-start delete-start
         new-insert-length (- edit-start insert-start)
         new-delete-start (+ insert-start insert-length)
         new-delete-length (- (+ delete-start delete-length) new-delete-start)
         edit-length (- new-delete-start edit-start)]
     [[:insert {:start insert-start
                :length new-insert-length}]
      [:edit {:start edit-start
              :length edit-length}]
      [:delete {:start new-delete-start
                :length new-delete-length}]])

   ;;    ++++       =>
   ;; ----------        ---^^^^---
   (< delete-start insert-start (+ insert-start insert-length) (+ delete-start delete-length))
   (let [edit-start insert-start
         edit-length insert-length
         left-delete-start delete-start
         left-delete-length (- insert-start delete-start)
         right-delete-start (+ insert-start insert-length)
         right-delete-length (- (+ delete-start delete-length)
                                (+ insert-start insert-length))]
     [[:delete {:start left-delete-start
                :length left-delete-length}]
      [:edit {:start edit-start
              :length edit-length}]
      [:delete {:start right-delete-start
                :length right-delete-length}]])

   ;; ++++++++++    =>
   ;;    ----           +++^^^^+++
   (< insert-start delete-start (+ delete-start delete-length) (+ insert-start insert-length))
   (let [edit-start delete-start
         edit-length delete-length
         left-insert-start insert-start
         left-insert-length (- delete-start insert-start)
         right-insert-start (+ delete-start delete-length)
         right-insert-length (- (+ insert-start insert-length)
                                right-insert-start)]
     [[:insert {:start left-insert-start
                     :length left-insert-length}]
      [:edit {:start edit-start
              :length edit-length}]
      [:insert {:start right-insert-start
                :length right-insert-length}]])

   ;; +++++++
   ;;    ----   =>  +++^^^^
   ;;   or
   ;;    ++++
   ;; -------   =>  ---^^^^
   (= (+ insert-start insert-length) (+ delete-start delete-length))
   (if (< insert-start delete-start)
     [[:insert {:start insert-start
               :length (- insert-length delete-length)}]
      [:edit {:start delete-start
             :length delete-length}]]
     [[:delete {:start delete-start
               :length (- delete-length insert-length)}]
      [:edit {:start insert-start
              :length insert-length}]])

   ;;(assert false hunk)

   ;; +++
   ;;      -----
   :else
   (into [] hunk)))





(defn remove-length-zero [hunk]
  (cond
   (zero? (-> hunk :insert :length)) (dissoc hunk :insert)
   (zero? (-> hunk :delete :length)) (dissoc hunk :delete)
   :else hunk))

(defn parse-hunk [hunk]
  (let [[_ deletions inserts] (s/split hunk #" ")
        [delete-start delete-length] (s/split deletions #",")
        [inserts-start inserts-length] (s/split inserts #",")]
    {:delete {:start (Long/parseLong (subs delete-start 1))
              :length (if delete-length
                        (Long/parseLong delete-length)
                        1)}
     :insert {:start (Long/parseLong (subs inserts-start 1))
              :length (if inserts-length
                        (Long/parseLong inserts-length)
                        1)}}))

(defn parse-chunk [[[hunk] lines]]
  {:hunk (resolve-overlap (remove-length-zero (parse-hunk hunk)))
   ;; We don't need the changes for now..
   ;; :lines lines
   })

(defn trim-filename [filename]
  (let [filename (subs filename 4)]
    (if (or (.startsWith filename "a/")
            (.startsWith filename "b/"))
      (subs filename 2)
      filename)))

(defn parse-chunks [[[from] [to & lines]]]
  {:from (trim-filename from)
   :to (trim-filename to)
   :chunks (->> lines
                (partition-by #(.startsWith % "@@"))
                (partition 2)
                (map parse-chunk))})

(defn parse-diff [[_ lines]]
  (->> lines
       (partition-by #(.startsWith % "---"))
       (drop 1)
       (partition 2)
       (map parse-chunks)))

(defn parse-diffs [lines]
  lines
  (->> lines
       (partition-by #(.startsWith % "diff --git"))
       (partition 2)
       (map parse-diff)
       (map first)
       (remove nil?) ;; Investigate: file perms commits might yield nil diffs
       ))

(defn parse-commit [[[commit] lines]]
  (let [commit (subs commit 7)
        [author date & lines] lines
        author (subs author 8)
        date (subs date 8)
        [commit-msg lines] (split-with #(not (.startsWith % "diff")) lines)]
    (let [res {:commit (subs commit 7)
               :author author
               :date date
               :commit-msg (s/trim (s/join "\n" (map s/trim commit-msg)))
               :diff (parse-diffs lines)}]
      res)
    ))

(defn parse-log [user repo]
  (->> (git-log-cmd user repo)
       sh
       (partition-by #(.startsWith % "commit"))
       (partition 2)
       (map parse-commit)))

;; A few invariants, checked with assert statements
;; * the only time :to and :from differs is when one of them is /dev/null
;; * if :from == /dev/null there is exactly one :insert hunk
;;    this means that the file in :to was created
;; * if :to  == /dev/null there is exactly one :delete hunk
;;    this means that the file in :from was deleted



(defn insert [file hvs start length]
  (update-in hvs [file] splice (dec start) (vec-of length 1)))

(defn edit [file hvs start length]
  (update-in hvs [file] map-part (dec start) length inc))

(defn delete [file hvs start length]
  (update-in hvs [file] cut (dec start) length))

(defn update-heat-vector-for [hvs file chunks]
  (let [hunks (map :hunk chunks)]
    (try (reduce (fn [hvs hunk]
                   (reduce (fn [hvs [op {:keys [start length]}]]
                             (condp = op
                               :insert (insert file hvs start length)
                               :edit (edit file hvs start length)
                               :delete (delete file hvs start length)))
                           hvs
                           hunk))
                 hvs
                 hunks)
         (catch IndexOutOfBoundsException e
           (try  (reduce (fn [hvs hunk]
                           (reduce (fn [hvs [op {:keys [start length]}]]
                                     (condp = op
                                       :insert (insert file hvs start length)
                                       :edit (edit file hvs start length)
                                       :delete (delete file hvs start length)))
                                   hvs
                                   hunk))
                         hvs
                         (reverse hunks))
                 (catch IndexOutOfBoundsException e
                   ;; (println "IOOBE")
                   hvs))))))

;;  (build-heat-vectors (parse-log "jonase" "cljsfiddle"))

;; (splice  (vec-of 1073 1) 15 (vec-of 2 1))

(defn update-heat-vectors [hvs diff]
  (try (cond
        (= (:from diff) "/dev/null")
        (do
          (assoc hvs (:to diff) (v/vec (repeat (-> diff :chunks first :hunk first second :length) 1))))

        (= (:to diff) "/dev/null")
        (dissoc hvs (:from diff))

        :else
        (update-heat-vector-for hvs (:to diff) (:chunks diff)))
       (catch IllegalArgumentException e
         ;; (println "IAE")
         hvs)))

(defn build-heat-vectors [commits]
  (reduce (fn [commits commit]
            (assert (not (some nil? (:diff commit))) commit)
            (let [heatvecs (:heatvecs (last commits) {})]
              (conj commits
                    (assoc commit
                      :heatvecs  (reduce (fn [heat-vectors diff]
                                           (update-heat-vectors heat-vectors diff))
                                         heatvecs
                                         (:diff commit)))))
            )
          []
          commits))

(defn build-tx [{:keys [commit heatvecs]}]
  (for [[file hv] heatvecs]
    {:file/commit commit
     :file/path file
     :file/heatmap (json/generate-string hv)})
  )


;; To get to the last commit do
;;  (last (build-heat-vectors  (parse-log "clojure" "clojurescript")))
;; (build-tx (last (build-heat-vectors  (parse-log "clojure" "clojurescript"))))

(comment
  (set! *print-length* 10)




  (some #(empty? (:diff %)) (parse-log "clojure" "clojurescript"))


  (take 10 (sh (git-log-cmd "clojure" "clojurescript"))))


(comment
  ;; INVESTIGATE!
;;   AssertionError Assert failed: ["(\"diff --git a/bin/cljsc.bat b/bin/cljsc.bat\" \"old mode 100755\" \"new mode 100644\" \"diff --git a/script/repl.bat b/script/repl.bat\" \"old mode 100755\" \"new mode 100644\" \"diff --git a/script/repljs.bat b/script/repljs.bat\" \"old mode 100755\" \"new mode 100644\" \"\")" "{:commit \"141131736871e791918df63f185155421\", :author \"David Nolen <dnolen@Davids-MacBook-Pro.local>\", :date \"Wed Mar 14 20:21:07 2012 -0400\", :commit-msg \"* bin/cljsc.bat: CLJS-159: fix file perms for Windows scripts\", :diff (nil nil nil)}"]

)

(comment
  [[:insert {:start 16, :length 2}]]
  [[:insert {:start 86, :length 13}]]
  [[:delete {:start 631, :length 1}]
   [:insert {:start 646, :length 1}]]
  [[:delete {:start 650, :length 1}]
   [:insert {:start 665, :length 1}]]
  )


(comment                             177
  [[:delete {:start 21, :length 1}]] 176
  [[:delete {:start 34, :length 1}]] 175
  [[:delete {:start 50, :length 1}]] 174
  [[:delete {:start 66, :length 1}]] 173
  [[:delete {:start 73, :length 1}]] 172
  [[:delete {:start 79, :length 1}]] 171
  [[:delete {:start 91, :length 1}]] 170
  [[:delete {:start 98, :length 1}]] 169
  [[:delete {:start 108, :length 1}]] 168
  [[:delete {:start 115, :length 1}]] 167
  [[:delete {:start 144, :length 2}]] 166
  [[:delete {:start 152, :length 2}]] 165
  [[:delete {:start 175, :length 2}]  164 X
   [:insert {:start 161, :length 1}]]
())


(comment                               414
  [[:delete {:start 408, :length 1}]   413
   [:insert {:start 418, :length 1}]]  X
  [[:delete {:start 388, :length 1}]
   [:insert {:start 398, :length 1}]]
  [[:delete {:start 378, :length 1}]
   [:insert {:start 388, :length 1}]]
  [[:edit {:start 299, :length 9}]
   [:insert {:start 308, :length 10}]]
  [[:edit {:start 266, :length 1}]]
  [[:edit {:start 166, :length 1}]]
  [[:edit {:start 159, :length 1}]]

  )
