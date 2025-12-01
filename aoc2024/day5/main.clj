(require '[clojure.string :as str])

(defn parse-rules
  "Transforms given lines into precedence map
   Example:
      1|2
      1|4
      3|4
      2|4
      => {1 #{2 4}, 3 #{4}, 2 #{4}}
  "
  [lines]
  (->> lines
       (mapv #(str/split % #"\|"))
       (mapv #(mapv Integer/parseInt %))
       (map (fn [[k v]] {k #{v}})) ; transform each [k v] into {k #{v}}
       (apply merge-with into)))   ; merge all those maps with `into` merge function

(defn parse-updates
  "Transforms given lines into vector of vector of ints (aka updates)"
  [lines]
  (->> lines
       (mapv #(str/split % #","))
       (mapv #(mapv Integer/parseInt %))))

(defn read-input
  [filename]
  (let [lines (str/split-lines (slurp filename))
        part-end (.indexOf lines "")]
    [(subvec lines 0 part-end) (subvec lines (inc part-end))]))

(defn valid-update?
  "Checks if given update (vector of ints) is valid according to rules map"
  [rules update]
  (let [inverted (zipmap update (range))]
    (every? (fn [number]
              (every? #(or (not (inverted %))
                           (> (inverted %) (inverted number)))
                      (rules number #{})))
            update)))

; First part of Day 5
(let [[rules-lines update-lines] (read-input "input.txt")
      rules (parse-rules rules-lines)
      updates (parse-updates update-lines)]
  (->> updates
       (filter #(valid-update? rules %))
       (map #(nth % (quot (count %) 2)))
       (apply +)))

; Second part of Day 5
(defn rules-comparator
  "Simple comparator for sorting, that checks precedence in rules map"
  [rules]
  (fn [x y] (contains? (rules x) y)))

(let [[rules-lines update-lines] (read-input "input.txt")
      rules (parse-rules rules-lines)
      updates (parse-updates update-lines)]
  (->> updates
       (remove (partial valid-update? rules))
       (map (fn [update] (sort-by identity (rules-comparator rules) update)))
       (map #(nth % (quot (count %) 2)))
       (apply +)))
