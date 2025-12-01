(require '[clojure.string :as str])

(defn read-matrix
  "Reads symbols matrix from file"
  [filename]
  (->> (slurp filename)
       str/split-lines
       (map vec) ; convert each line to vector of character
       vec))

(defn get-2d "Get value of m[x][y]" [m [x y]] ((m y) x))

(defn valid
  "Checks whether (x, y) is a valid point in `m`"
  [m [x y]]
  (and (>= x 0) (>= y 0) (< y (count m)) (< x (count (m 0)))))

(defn check-xmas
  "Checks whether given points in `m` are constructing word XMAS"
  [m points]
  (and (every? (partial valid m) points)
       (= "XMAS" (apply str (map #(get-2d m %) points)))))

(defn xmas-directions
  "Creates 8 points arrays of length 4,
   for each direction (2 for verical, 4 for diagonal, 2 for horizontal)"
  [[x y]]
  (let [deltas [[1 0] [-1 0] [0 1] [0 -1] [-1 -1] [-1 1] [1 -1] [1 1]]]
    (for [delta deltas]
      (for [step (range 4)]
        [(+ x (* step (delta 0))) (+ y (* step (delta 1)))]))))

(defn count-xmas
  "Counts number of XMAS words in given matrix"
  [m]
  (let [width (count (m 0))
        height (count m)]
    (->> (for [x (range width)
               y (range height)]
           (map #(check-xmas m %) (xmas-directions [x y])))
         flatten
         (filter identity)
         count)))

(defn get-main-diag
  "Returns points vector of length 3 of main diagonal
   with center located in given coordinates"
  [m x y]
  [(get-2d m [(- x 1) (- y 1)]) (get-2d m [x y]) (get-2d m [(+ x 1) (+ y 1)])])

(defn get-sec-diag
  "Returns points vector of length 3 of secondary diagonal
   with center located in given coordinates"
  [m x y]
  [(get-2d m [(+ x 1) (- y 1)]) (get-2d m [x y]) (get-2d m [(- x 1) (+ y 1)])])

(defn check-mas
  "Checks wheter there are two MAS/SAM words in main and secondary diagonal
   with center located in given coordinates"
  [m [x y]]
  (let [main-diag-str (apply str (get-main-diag m x y))
        sec-diag-str (apply str (get-sec-diag m x y))]
    (and (or (= main-diag-str "MAS") (= main-diag-str "SAM"))
         (or (= sec-diag-str "MAS") (= sec-diag-str "SAM")))))

(defn count-mas
  "Counts number of MAS/SAM crosses in given matrix"
  [m]
  (let [width (count (m 0))
        height (count m)]
    (->> (for [x (range 1 (- width 1)) y (range 1 (- height 1))] [x y])
         (filter #(check-mas m %))
         count)))

; First task of Day 4
(count-xmas (read-matrix "input.txt"))

; Second task of Day 4
(count-mas (read-matrix "input.txt"))
