(defn parse-input []
  (->> (slurp "input13.txt")
       (re-seq #"Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)")
       (mapv rest)
       (mapv #(mapv parse-long %))))

;; form is ax + by = c
;;         dx + ey = f
(defn solve-binary-system [e1 e2]
  (let [[a b c] e1
        [d e f] e2]
    (assert (not (zero? a)))
    (assert (not (zero? b)))
    (assert (not (zero? d)))
    (assert (not (zero? e)))
    (let [scaled-e2 (mapv #(* % (/ a d)) e2)
          [_ b' c'] (mapv - e1 scaled-e2)]
      (cond
        (and (zero? b') (zero? c')) :infinite-solutions
        (zero? b') :no-solution
        :else (let [y (/ c' b')
                    x (/ (- c (* b y)) a)]
                [x y])))))

(comment
  (solve-binary-system [94 22 8400]
                       [34 67 5400])
  (solve-binary-system [26 67 12748]
                       [66 21 12176])
  (solve-binary-system [17 84 7870]
                       [86 37 6450])
  (solve-binary-system [17 27 18641]
                       [23 71 10279]))

(defn best [[ax ay bx by x y]]
  (let [sln (solve-binary-system [ax bx x] [ay by y])]
    (cond
      (= sln :no-solution) 0
      ;; We're lucky they don't use this
      (= sln :infinite-solutions) (throw (ex-info "Unimplemented" {}))
      :else (let [[a-presses b-presses] sln]
              (cond
                (neg? a-presses) 0
                (neg? b-presses) 0
                (not (integer? a-presses)) 0
                (not (integer? b-presses)) 0
                :else (+ (* 3 a-presses) b-presses))))))

(comment
  (best [17 86 84 37 7870 6450]))

(defn part1 [machines]
  (->> (mapv best machines)
       (reduce +)))

(defn part2 [machines]
  (->> machines
       (mapv (fn [[ax ay bx by x y]]
               [ax ay bx by (+ 10000000000000 x) (+ 10000000000000 y)]))
       (mapv best)
       (reduce +)))

(comment
  (part1 (parse-input))
  (part2 (parse-input)))
