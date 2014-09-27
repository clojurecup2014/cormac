(ns cormac.algo
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
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
     [[:insert-left {:start left-insert-start
                     :length left-insert-length}]
      [:edit {:start edit-start
              :length edit-length}]
      [:insert-right {:start right-insert-start
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
       (map first)))

(defn parse-commit [[[commit] lines]]
  (let [commit (subs commit 7)
        [author date & lines] lines
        author (subs author 8)
        date (subs date 8)
        [commit-msg lines] (split-with #(not (.startsWith % "diff")) lines)]
    {:commit (subs commit 7)
     :author author
     :date date
     :commit-msg (s/trim (s/join "\n" (map s/trim commit-msg)))
     :diff (parse-diffs lines)}))

(defn parse-log [user repo]
  (->> (git-log-cmd user repo)
       sh
       (partition-by #(.startsWith % "commit"))
       (partition 2)
       (map parse-commit)))

(comment
  (nth (parse-log "clojure" "clojurescript") 1207)

  (take 10 (sh (git-log-cmd "clojure" "clojurescript"))))
