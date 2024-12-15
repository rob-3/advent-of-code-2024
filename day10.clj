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

(defn score-trailhead [& {:keys [start neighbors-fn distinct?]}]
  (let [dedup (if distinct? identity set)]
    (loop [i 0
           locations #{start}]
      (if (= i 9)
        (count locations)
        (recur (inc i) (->> locations
                            (mapcat neighbors-fn)
                            dedup))))))

(defn walk-trailheads [grid distinct?]
  (reduce + (for [y (range (count grid))
                  x (range (count (get grid 0)))]
              (if (= 0 (get-in grid [y x]))
                (score-trailhead :start [y x]
                                 :neighbors-fn #(trail-neighbors % grid)
                                 :distinct? distinct?)
                0))))

(defn part1 [grid]
  (walk-trailheads grid false))

(defn part2 [grid]
  (walk-trailheads grid true))

(comment
  (part1 (parse-input))
  (part2 (parse-input)))
