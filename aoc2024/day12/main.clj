(require '[clojure.string :as str])
(require '[clojure.set])

; Only for testing
(defrecord Region [letter points])

(defn valid?
  [m [x y]]
  (let [width (count (m 0))
        height (count m)]
    (and (>= x 0) (>= y 0) (< x width) (< y height))))

(defn surrounding [[x y]] [[(+ x 1) y] [(- x 1) y] [x (+ y 1)] [x (- y 1)]])
(defn surrounding-match
  [m [x y]]
  (vec (filter #(and (valid? m %) (= (get-in m [y x]) (get-in m (reverse %))))
         (surrounding [x y]))))

(defn perimiter
  [points]
  (let [points (set points)]
    (apply +
      (map (fn [[x y]] (count (remove #(points %) (surrounding [x y]))))
        points))))

(defn area [points] (count points))

(defn first-unvisited
  [visited]
  (first (for [x (range (count (get visited 0)))
               y (range (count visited))
               :when (not (get-in visited [y x]))]
           [x y])))

(defn bfs
  [m visited points]
  (if (empty? points) ;; If no more points left, exit
    (->Region (get-in m (reverse (first visited))) visited)
    ;; For every point in points, add them into visited. Then find all
    ;; their MATCHING surrounding points And run recursively (recur) this
    ;; function with new visited and new points
    (let [surr (filter #(not (visited %))
                 (reduce into [] (map #(surrounding-match m %) points)))]
      (recur m (reduce conj visited points) surr))))

(defn traverse
  [m]
  (loop [m m
         notvisited (set
                      (for [x (range (count (m 0))) y (range (count m))] [x y]))
         regions []]
    (if (empty? notvisited)
      regions
      (let [region (bfs m #{} [(first notvisited)])]
        (recur m
               (clojure.set/difference notvisited (:points region))
               (conj regions region))))))

;; First part
; (let [m (->> (slurp "input.txt")
;              str/split-lines
;              (mapv vec))
;       regions (traverse m)
;       price (apply +
;               (map *
;                 (map area (map :points regions))
;                 (map perimiter (map :points regions))))]
;   price)

;; Second part
(defn has-up [points-set [x y]] (not (points-set [x (- y 1)])))
(defn has-down [points-set [x y]] (not (points-set [x (+ y 1)])))
(defn has-left [points-set [x y]] (not (points-set [(- x 1) y])))
(defn has-right [points-set [x y]] (not (points-set [(+ x 1) y])))

(defn scan-up
  [points]
  (let [points-set (set points)
        upper (apply min (map second points))
        lower (apply max (map second points))]
    (apply +
      (map #(inc (count %))
        (map #(filter (fn [c] (>= c 2)) %)
          (map #(map - (rest %) %)
            (map sort
              (filter seq
                (for [y (range upper (inc lower))]
                  (let [level-points (filter #(= y (second %)) points)]
                    (map first
                      (filter #(has-up points-set %) level-points))))))))))))

(defn scan-down
  [points]
  (let [points-set (set points)
        upper (apply min (map second points))
        lower (apply max (map second points))]
    (apply +
      (map #(inc (count %))
        (map #(filter (fn [c] (>= c 2)) %)
          (map #(map - (rest %) %)
            (map sort
              (filter seq
                (for [y (range upper (inc lower))]
                  (let [level-points (filter #(= y (second %)) points)]
                    (map first
                      (filter #(has-down points-set %) level-points))))))))))))

(defn scan-left
  [points]
  (let [points-set (set points)
        left (apply min (map first points))
        right (apply max (map first points))]
    (apply +
      (map #(inc (count %))
        (map #(filter (fn [c] (>= c 2)) %)
          (map #(map - (rest %) %)
            (map sort
              (filter seq
                (for [x (range left (inc right))]
                  (let [level-points (filter #(= x (first %)) points)]
                    (map second
                      (filter #(has-left points-set %) level-points))))))))))))

(defn scan-right
  [points]
  (let [points-set (set points)
        left (apply min (map first points))
        right (apply max (map first points))]
    (apply +
      (map #(inc (count %))
        (map #(filter (fn [c] (>= c 2)) %)
          (map #(map - (rest %) %)
            (map sort
              (filter seq
                (for [x (range left (inc right))]
                  (let [level-points (filter #(= x (first %)) points)]
                    (map second
                      (filter #(has-right points-set %) level-points))))))))))))

(defn sides
  [points]
  (+ (scan-left points)
     (scan-right points)
     (scan-up points)
     (scan-down points)))

(defn price
  [region]
  (assoc region
    :price (* (area (:points region)) (sides (:points region)))
    :area (area (:points region))
    :sides (sides (:points region))))

(let [m (->> (slurp "input.txt")
             str/split-lines
             (mapv vec))
      regions (traverse m)
      prices (map price regions)]
  (apply + (map :price prices)))

