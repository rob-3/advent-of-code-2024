(defn parse-input []
  (->> (slurp "input3.txt")
       (re-seq #"mul\((-?\d+),(-?\d+)\)")))

(defn part1 [mul-exprs]
  (->> (for [[_ x y] mul-exprs]
        (* (parse-long x) (parse-long y)))
      (reduce +)))

(defn parse-input-2 []
  (->> (slurp "input3.txt")
       (re-seq #"mul\((-?\d+),(-?\d+)\)|do\(\)|don't\(\)")))

(defn part2 [mul-exprs]
  (loop [exprs mul-exprs
         sum 0
         on? true]
    (if (= 0 (count exprs))
      sum
      (let [[expr x y] (first exprs)]
        (condp = expr
          "don't()" (recur (rest exprs) sum false)
          "do()" (recur (rest exprs) sum true)
          (if on?
            (recur (rest exprs) (+ sum (* (parse-long x) (parse-long y))) true)
            (recur (rest exprs) sum false)))))))

(part1 (parse-input))
(part2 (parse-input-2))
