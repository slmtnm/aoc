(require '[clojure.string :as str])

(defn read-input
  "Reads input to [[target1 numbers1] [target2 numbers2] ...]"
  [filename]
  (->> (slurp filename)
       str/split-lines
       (map #(str/split % #":"))
       (map #(map str/trim %))
       (map #(vector (parse-long (first %))
                     (mapv parse-long (str/split (second %) #" "))))))

(defn can-be-combined?
  [target values current]
  (if (empty? values)
    (= current target)
    (let [v (first values)]
      (and (<= current target)
           (or (can-be-combined? target (rest values) (* current v))
               (can-be-combined? target (rest values) (+ current v)))))))

(defn concat-longs [a b] (parse-long (str a b)))

(defn can-be-combined-concat?
  [target values current]
  (if (empty? values)
    (= current target)
    (let [v (first values)]
      (and (<= current target)
           (or (can-be-combined-concat? target (rest values) (* current v))
               (can-be-combined-concat? target (rest values) (+ current v))
               (can-be-combined-concat? target
                                        (rest values)
                                        (concat-longs current v)))))))

; First part
(->> (read-input "input.txt")
     (filter (fn [[target values]]
               (can-be-combined? target (rest values) (first values))))
     (map first)
     (apply +))

; Second part
(->> (read-input "input.txt")
     (filter (fn [[target values]]
               (can-be-combined-concat? target (rest values) (first values))))
     (map first)
     (apply +))

