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

;; (nth (parse-log "clojure" "clojurescript") 1207)

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
  {:hunk (parse-hunk hunk)
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
