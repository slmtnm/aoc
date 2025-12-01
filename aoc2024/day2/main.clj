(require '[clojure.string :as str])

(defn read-matrix
  "Reads file `filename` and creates matrix of int's
   Return type is list of lists of ints"
  [filename]
  (->> (slurp filename)
       str/split-lines
       (map (fn [line]
              (->> (str/split line #" ")
                   (map Integer/parseInt))))))

(defn subseq1
  "Generates all subvectors of length (N - 1) of given vector `v` where N = (count line)
   Example: (subseq1 [1 2 3 4]) -> [[2 3 4] [1 3 4] [1 2 4] [1 2 3]]"
  ([v] (subseq1 v 0 []))
  ([v n acc]
   (if (= n (count v))
     acc
     (subseq1 v (inc n) (conj acc (into (subvec v 0 n) (subvec v (inc n))))))))

(defn monotonic3?
  "Checks whether given vector `v` is strictly monotonic,
   with difference between adjucent items not greater than 3"
  [v]
  (let [diffs (map - (rest v) v)]
    (or (every? #(and (> % 0) (<= % 3)) diffs)
        (every? #(and (< % 0) (>= % -3)) diffs))))

(defn monotonic3-tolerate?
  "Checks whether given vector `v` is strictly monotonic
   OR is strictly monotonic after removing one element from it"
  [v]
  (boolean (some monotonic3? (subseq1 (vec v)))))

; First part of Day 2's task
(defn aoc1 [filename] (count (filter monotonic3? (read-matrix filename))))

; Second part of Day 2's task
(defn aoc2
  [filename]
  (count (filter monotonic3-tolerate? (read-matrix filename))))

(aoc1 "input.txt")
(aoc2 "input.txt")
