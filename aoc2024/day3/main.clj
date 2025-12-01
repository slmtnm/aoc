(require '[clojure.string :as str])

; Pattern of "mul(x, y)" expression, where x and y are integer numbers
(def mulpat (re-pattern #"mul\(([0-9]+),([0-9]+)\)"))

; Pattern of "don't()......do()" expression
(def dontpat (re-pattern #"(don't\(\)).*?(do\(\)|$)"))

(defn execute
  "Executes given code"
  [code]
  (->> code
       (re-seq mulpat)
       (map rest)
       (map #(map Integer/parseInt %))
       (map (partial apply *))
       (reduce +)))

; First task of Day 3
(->> (slurp "input.txt")
     execute)

; Second task of Day 3
(-> (slurp "input.txt")
    str/split-lines
    str/join
    (str/replace dontpat "")
    execute)

