(require '[clojure.string :as str])

; Helpers
(defn vec2-sub [v1 v2] [(- (first v1) (first v2)) (- (second v1) (second v2))])
(defn vec2-add [v1 v2] [(+ (first v1) (first v2)) (+ (second v1) (second v2))])
(defn vec2-negate [v] [(- 0 (first v)) (- 0 (second v))])
(defn valid?
  "Checks whether given point is in bound of [0:w][0:h]"
  [[x y] w h]
  (and (>= x 0) (>= y 0) (< x w) (< y h)))

(defn antinodes
  "Get two antinodes of given two antenns
   Not guaranteed that returned points are valid"
  [v1 v2]
  (let [delta (vec2-sub v2 v1)] [(vec2-add v2 delta) (vec2-sub v1 delta)]))

(defn walk
  [start delta w h pointset]
  (if-not (valid? start w h)
    pointset
    (recur (vec2-add start delta) delta w h (conj pointset start))))

(defn antinodes-line
  "Get antinodes of given two antenns
   Nt guaranteed that returned points are valid"
  [w h v1 v2]
  (let [delta (vec2-sub v2 v1)
        forward (walk v1 delta w h #{})
        backward (walk v1 (vec2-negate delta) w h #{})]
    (reduce conj forward backward)))

(defn read-matrix
  "Reads file into matrix of characters"
  [filename]
  (->> (slurp filename)
       str/split-lines
       (mapv vec)))

(defn find-antennas
  "With given matrix `m`, finds all antennas in it and
   creates mapping from frequency to vector of
   antenna's positions with that frequency"
  [m]
  (let [w (count (m 0))
        h (count m)]
    (apply merge-with
      into
      (for [x (range w)
            y (range h)
            :let [chr (get-in m [y x])]
            :when (not= \. chr)]
        {chr #{[x y]}}))))

(defn pairs
  "Returns all distinct pairs in sequence"
  [seq]
  (loop [seq seq
         total []]
    (if (empty? seq)
      total
      (recur (rest seq)
             (into []
                   (concat total (map #(vector (first seq) %) (rest seq))))))))

; First task
(let [m (read-matrix "input.txt")
      w (count (m 0))
      h (count m)]
  (->> m
       find-antennas
       (map val)
       (map pairs)
       (reduce into)
       (map #(apply antinodes %))
       (reduce into)
       set
       (filter #(valid? % w h))
       count))

; Second task
(let [m (read-matrix "input.txt")
      w (count (m 0))
      h (count m)]
  (->> m
       find-antennas
       (map val)
       (map pairs)
       (reduce into)
       (map #(apply antinodes-line w h %))
       (reduce into)
       set
       (filter #(valid? % w h))
       count))
