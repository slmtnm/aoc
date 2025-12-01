(require '[clojure.string :as str])
(require '[clojure.set])

(defn char-to-int
  "Converts character to int"
  [chr]
  (Integer/parseInt (str chr)))

(defn read-matrix
  "Reads file to 2d matrix of ints"
  [filename]
  (->> (slurp filename)
       str/split-lines
       (map vec)
       (mapv #(mapv char-to-int %))))

(defn get-2d "Get m[x][y]" [m [x y]] ((m y) x))

(defn find-trailheads
  "Finds all zero values in 2d matrix"
  [m]
  (for [x (range (count (m 0)))
        y (range (count m))
        :when (= 0 (get-in m [y x]))]
    [x y]))

(defn valid?
  "Checks if [x y] is a valid point in matrix (in bounds)"
  [m [x y]]
  (let [width (count (m 0))
        height (count m)]
    (and (>= x 0) (>= y 0) (< x width) (< y height))))

(defn surrounding
  "Finds all valid surronding points if [x y] in given matrix"
  [m [x y]]
  (filter #(valid? m %) [[(+ x 1) y] [(- x 1) y] [x (+ y 1)] [x (- y 1)]]))

(defn score
  "Calculates score of trailhead"
  [m point]
  (if (= (get-2d m point) 9)
    #{point}
    (let [next-points (filter #(= (- (get-2d m %) 1) (get-2d m point))
                        (surrounding m point))]
      (apply clojure.set/union (map #(score m %) next-points)))))

(defn rating
  "Calculates rating of trailhead"
  [m point]
  (if (= (get-2d m point) 9)
    1
    (let [next-points (filter #(= (- (get-2d m %) 1) (get-2d m point))
                        (surrounding m point))]
      (apply + (map #(rating m %) next-points)))))

; First task
(apply +
  (let [m (read-matrix "input.txt")]
    (map #(count (score m %)) (find-trailheads m))))

; First task
(apply +
  (let [m (read-matrix "input.txt")] (map #(rating m %) (find-trailheads m))))
