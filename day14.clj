(defn parse-input []
  (->> (slurp "input14.txt")
       (re-seq #"p=(\d+),(\d+) v=(-?\d+),(-?\d+)")
       (mapv rest)
       (mapv #(mapv parse-long %))
       (mapv #(partitionv 2 %))))

(defn tick [[[x y] [dx dy]]]
  [[(mod (+ x dx) 101) (mod (+ y dy) 103)] [dx dy]])

(defn safety-factor [state]
  (->> (for [[[x y]] state]
         (cond
           (and (< x 50) (< y 51)) :quad1
           (and (> x 50) (< y 51)) :quad2
           (and (< x 50) (> y 51)) :quad3
           (and (> x 50) (> y 51)) :quad4))
       (remove nil?)
       frequencies
       vals
       (reduce *)))

(defn part1 [state]
  (-> (iterate #(mapv tick %) state)
      (nth 100)
      safety-factor))

(comment
  (part1 (parse-input)))
