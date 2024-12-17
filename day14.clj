(defn parse-input []
  (->> (slurp "input14.txt")
       (re-seq #"p=(\d+),(\d+) v=(-?\d+),(-?\d+)")
       (mapv rest)
       (mapv #(mapv parse-long %))
       (mapv #(partitionv 2 %))))

(def width 101)
(def height 103)

(defn tick [[[x y] [dx dy]]]
  [[(mod (+ x dx) width) (mod (+ y dy) height)] [dx dy]])

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

(defn part2 [state]
  (let [states (iterate #(mapv tick %) state)]
    (loop [i 0]
      (print i)
      (let [state-set (set (mapv first (nth states (+ 90 (* 103 i)))))]
        (doseq [y (range height)
                x (range width)]
          (when (= 0 x) (println))
          (if (state-set [x y])
            (print "*")
            (print " "))))
      (if (= (read-line) "q")
        nil
        (recur (inc i))))))

(comment
  (part1 (parse-input))
  (part2 (parse-input)))
