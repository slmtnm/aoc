(require '[clojure.string :as str])

(defn read-input
  [filename]
  (->> (-> (slurp filename)
           str/trim
           (str/split #" "))
       (mapv parse-long)))

(defn blink
  "Transform stone into one or more (vector) stones
   using blink rules"
  [stone]
  (cond (= stone 0) [1]
        (even? (count (str stone)))
          [(parse-long (subs (str stone) 0 (/ (count (str stone)) 2)))
           (parse-long (subs (str stone) (/ (count (str stone)) 2)))]
        :else [(* stone 2024)]))

(defn blink-iterate
  "Make N blinks on given stone vector"
  [stones n]
  (if (= n 0)
    (count stones)
    (recur (vec (flatten (map blink stones))) (dec n))))

; First part
(blink-iterate (read-input "input.txt") 40)

; Second part
(def blink-iterate-memo
  "Same as blink-iterate but memoize answers"
  (memoize (fn [current n]
             (if (= n 0)
               (count current)
               (let [nextcurrent (vec (flatten (map blink current)))]
                 (apply +
                   (map #(blink-iterate-memo [%] (dec n)) nextcurrent)))))))

(blink-iterate-memo (read-input "input.txt") 75)
