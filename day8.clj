(require '[clojure.string :as str])

(defn parse-input []
  (as-> (slurp "input8.txt") $
    (str/split $ #"\n")
    (mapv vec $)))

(defn antenna-locations [grid]
  (let [x-max (count (get grid 0))]
    (loop [i 0
           antenna->locations {}]
      (let [[y x] [(quot i x-max) (mod i x-max)]
            c (get-in grid [y x])]
        (case c
          \. (recur (inc i) antenna->locations)
          nil antenna->locations
          (recur (inc i) (update antenna->locations c (fnil conj #{}) [y x])))))))

(defn for-antenna-pairs [grid f]
  (let [antenna->locations (antenna-locations grid)]
    (->> (for [locations (vals antenna->locations)]
           (for [a locations
                 b locations
                 :when (not= a b)]
             (f a b)))
         flatten
         (partition 2)
         set
         count)))

(defn part1 [grid]
  (let [in-grid? #(boolean (get-in grid %))]
    (for-antenna-pairs
     grid
     (fn [[ay ax] [by bx]]
       (let [[dy dx] [(- ay by) (- ax bx)]]
         (filterv in-grid? [[(+ ay dy) (+ ax dx)]
                            [(- by dy) (- bx dx)]]))))))

(defn part2 [grid]
  (let [in-grid? #(boolean (get-in grid %))]
    (for-antenna-pairs
     grid
     (fn [[ay ax] [by bx]]
       (let [[dy dx] [(- ay by) (- ax bx)]
             antinodes1 (for [i (drop 1 (range))
                              :let [y (+ ay (* dy i))
                                    x (+ ax (* dx i))]
                              :while (in-grid? [y x])]
                          [y x])
             antinodes2 (for [i (map - (drop 1 (range)))
                              :let [y (+ ay (* dy i))
                                    x (+ ax (* dx i))]
                              :while (in-grid? [y x])]
                          [y x])]
         (concat antinodes1 antinodes2))))))

(comment
  (antenna-locations (parse-input))
  (part1 (parse-input))
  (part2 (parse-input)))
