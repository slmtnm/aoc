(require '[clojure.string :as str])

; Helpers
(defn add-vec "Add two vectors" [v1 v2] (mapv + v1 v2))
(defn get-2d "Get m[x][y]" [m [x y]] (get-in m [y x]))
(defn set-2d "Set m[x][y] = val" [m [x y] val] (assoc-in m [y x] val))

(defn read-map
  "Read file to 2D matrix of symbols"
  [filename]
  (->> (slurp filename)
       str/split-lines
       (mapv vec)))

(def caret-direction
  "Mapping from caret symbol to vector direction"
  {\^ [0 -1], \> [1 0], \v [0 1], \< [-1 0]})

(defn find-guard
  "Find the caret in the matrix and return its position and direction."
  [m]
  (first (for [y (range (count m))
               x (range (count (m 0)))
               :let [dir (caret-direction (get-2d m [x y]))]
               :when dir]
           {:position [x y], :direction dir})))

(defn rotate
  "Rotate guard to the right"
  [guard]
  (case (:direction guard)
    [0 -1] (assoc guard :direction [1 0])
    [1 0] (assoc guard :direction [0 1])
    [0 1] (assoc guard :direction [-1 0])
    [-1 0] (assoc guard :direction [0 -1])))

(defn step
  "Make guard step forward"
  [guard]
  (update guard :position add-vec (:direction guard)))

(defn run
  "Run guard from staring position `guard`, in given matrix `m`
   Returns set of visited guard positions (alongside directions)
   or nil if there is a loop"
  [m visited guard]
  (let [next-position (add-vec (:position guard) (:direction guard))
        next-char (get-2d m next-position)
        new-visited (conj visited guard)]
    (cond (contains? visited guard) nil ; loop found
          (= next-char nil) new-visited ; end of matrix, guard finished
          (= next-char \#) (recur m new-visited (rotate guard)) ; rotate
          :else (recur m new-visited (step guard))))) ; just go forward

; First part of Day 6
(let [m (read-map "input.txt")]
  (->> (run m #{} (find-guard m))
       (map :position)
       set
       count))

; Second part of Day 6
(let [m (read-map "input.txt")
      guard (find-guard m)
      obstacles (disj (set (map :position (run m #{} guard)))
                      (:position guard))]
  (->> obstacles
       ; remain only those obstacles, that generate loop (run returns nil)
       (remove #(run (set-2d m % \#) #{} guard))
       count))
