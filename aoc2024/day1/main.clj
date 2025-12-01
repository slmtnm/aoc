(require '[clojure.string :as str])

(defn split-lists
  "Reads input file `filename`, splits each line by whitespace
   and constructs two sorted lists of int's"
  [filename]
  (let [pairs (map #(str/split % #"\s+") (str/split-lines (slurp filename)))
        first-list (sort (map (comp Integer/parseInt first) pairs))
        second-list (sort (map (comp Integer/parseInt second) pairs))]
    [first-list second-list]))

; First part of Day 1's task
(defn aoc1
  [filename]
  (apply +
    (map #(abs (- (first %) (second %)))
      (apply map vector (split-lists filename)))))

; Second part of Day 1's task
(defn aoc2
  [filename]
  (let [[first-list second-list] (split-lists filename)
        freq (frequencies second-list)]
    (apply + (map #(* % (get freq % 0)) first-list))))

(aoc1 "input.txt")
(aoc2 "input.txt")
