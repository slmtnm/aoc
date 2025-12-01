;; this code is shitty
(require '[clojure.string :as str])

(defn to-numbers [s] (mapv (comp Integer/parseInt str) (vec s)))

(defn mkdisk
  [numbers free id total]
  (let [chr (if free :empty id)]
    (if (empty? numbers)
      total
      (recur (rest numbers)
             (not free)
             (if free (inc id) id)
             (into [] (concat total (repeat (first numbers) chr)))))))

(defn find-rightmost
  [disk]
  (let [len (count disk)]
    (loop [disk (reverse disk)
           found nil
           i 0]
      (if (empty? disk)
        nil
        (cond (and found (= (first disk) :empty)) (- len 1 found)
              (and found (not= (first disk) :empty))
                (recur (rest disk) found (inc i))
              (and (not found) (= (first disk) :empty))
                (recur (rest disk) found (inc i))
              (and (not found) (not= (first disk) :empty))
                (recur (rest disk) i (inc i)))))))

(defn find-leftmost
  [disk]
  (first (first (filter #(= :empty (second %)) (map vector (range) disk)))))

(defn find-rightmost-file
  [disk]
  (let [len (count disk)]
    (loop [disk (reverse disk)
           found nil
           found-el nil
           i 0]
      (if (empty? disk)
        nil
        (cond (and found (not= (first disk) found-el)) [(- len i) (- len found)]
              (and found (not= (first disk) :empty))
                (recur (rest disk) found found-el (inc i))
              (and (not found) (= (first disk) :empty))
                (recur (rest disk) found found-el (inc i))
              (and (not found) (not= (first disk) :empty))
                (recur (rest disk) i (first disk) (inc i)))))))

(defn find-leftmost-file
  [disk n before]
  (loop [disk disk
         i 0]
    (if (= (take n disk) (repeat n :empty))
      [i (+ i n)]
      (if (>= i before) nil (recur (rest disk) (inc i))))))

(partition-by #(= :empty %) [1 :empty :empty 2 3 4])

(defn swap
  [disk i j]
  (assoc disk
    j (disk i)
    i (disk j)))

(defn swap-interval
  [disk src-start dst-start len]
  (into []
        (concat (subvec disk 0 src-start)
                (subvec disk dst-start (+ dst-start len))
                (subvec disk (+ src-start len) dst-start)
                (subvec disk src-start (+ src-start len))
                (subvec disk (+ dst-start len)))))

(defn dense
  [disk]
  (let [right (find-rightmost disk)
        left (find-leftmost disk)]
    (if (not right) disk (recur (swap disk left right)))))

(defn dense-file
  [disk backend]
  (let [right (find-rightmost-file (subvec disk 0 backend))]
    (if (not right)
      disk
      (let [len (- (second right) (first right))
            left (find-leftmost-file disk len (first right))]
        (if (seq left)
          (recur (swap-interval disk (first left) (first right) len)
                 (first right))
          (recur disk (first right)))))))

(defn checksum
  [disk]
  (apply +
    (map #(apply * %) (map vector (range) (map #(if (= :empty %) 0 %) disk)))))

; First part
(checksum (dense
            (mkdisk (to-numbers (str/trim (slurp "input.txt"))) false 0 [])))

; Second part
(let [disk (mkdisk (to-numbers (str/trim (slurp "input.txt"))) false 0 [])]
  (checksum (dense-file disk (count disk))))
