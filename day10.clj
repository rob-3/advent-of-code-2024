(defn parse-input []
  (->> (slurp "input10.txt")
       (re-seq #"\d+")
       (mapv #(re-seq #"\d" %))
       (mapv #(mapv parse-long %))))

(defn trail-neighbors [[y x] grid]
  (let [v (get-in grid [y x])]
    (->> [[(inc y) x]
          [y (inc x)]
          [(dec y) x]
          [y (dec x)]]
         (filterv #(= (inc v) (get-in grid %))))))

(defn score-trailhead [start neighbors-fn]
  (loop [i 0
         locations #{start}]
    (if (= i 9)
      (count locations)
      (recur (inc i) (->> locations
                          (mapcat neighbors-fn)
                          set)))))

(defn part1 [grid]
  (reduce + (for [y (range (count grid))
                  x (range (count (get grid 0)))]
              (if (= 0 (get-in grid [y x]))
                (score-trailhead [y x] #(trail-neighbors % grid))
                0))))

(comment
  (part1 (parse-input))
  (trail-neighbors [1 0] (parse-input))
  (score-trailhead [0 2] #(trail-neighbors % (parse-input)))
  (def grid (parse-input))
  (def locations #{[1 0]}))
