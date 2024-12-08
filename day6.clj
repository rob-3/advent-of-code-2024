(require '[clojure.string :as str])

(defn parse-input []
  (-> (slurp "input6.txt")
      (str/split #"\n")))

(defn starting-position [grid]
  (let [x-size (count (get grid 0))]
    (loop [i 0]
      (let [x (mod i x-size)
            y (quot i x-size)]
        (if (= \^ (get-in grid [y x]))
          [x y]
          (recur (inc i)))))))

(def turn-right
  {:up :right
   :right :down
   :down :left
   :left :up})

(defn part1 [grid]
  (let [[x y] (starting-position grid)]
    (loop [direction :up
           current-position [y x]
           visited? #{current-position}]
      (let [[y x] current-position
            next-position (case direction
                            :up [(dec y) x]
                            :down [(inc y) x]
                            :left [y (dec x)]
                            :right [y (inc x)])]
        (case (get-in grid next-position)
          \. (recur direction next-position (conj visited? next-position))
          \# (recur (turn-right direction) current-position visited?)
          nil (count visited?))))))

(defn parse-input-2 []
  (as-> (slurp "input6.txt") $
    (str/split $ #"\n")
    (mapv vec $)))

(defn part2 [grid]
  (let [[x y] (starting-position grid)
        path (loop [direction :up
                    current-position [y x]
                    grid (assoc-in grid current-position #{:up})]
               (let [[y x] current-position
                     next-position (case direction
                                     :up [(dec y) x]
                                     :down [(inc y) x]
                                     :left [y (dec x)]
                                     :right [y (inc x)])
                     p (get-in grid next-position)]
                 (cond
                   (= \. p) (recur direction next-position (assoc-in grid next-position #{direction}))
                   (= \# p) (let [direction' (turn-right direction)]
                              (recur direction' current-position (update-in grid current-position conj direction')))
                   (set? p) (recur direction next-position (update-in grid next-position conj direction))
                   :else grid)))]))

(comment
  (part1 (parse-input))
  (part2 (parse-input-2)))
