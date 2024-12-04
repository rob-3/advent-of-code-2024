(require '[clojure.string :as str])

(defn parse-input []
  (-> (slurp "input4.txt")
      (str/split #"\n")))

(defn count-xmas [s]
  (count (concat (re-seq #"XMAS" s)
                 (re-seq #"SAMX" s))))

(defn ltr-diagonals [lines]
  (let [len (count (first lines))
        diagonal-starts (concat (map vector (range len) (repeat 0))
                                (map vector (repeat 0) (drop 1 (range len))))]
    (mapv (fn [[x y]]
            (loop [n 0
                   s '()]
              (let [[x y] [(+ n x) (+ n y)]]
                (if-let [c (get-in lines [x y])]
                  (recur (inc n) (conj s c))
                  (apply str s)))))
          diagonal-starts)))

(defn all-diagonals [lines]
  (concat (ltr-diagonals lines)
          (ltr-diagonals (mapv str/reverse lines))))

(defn part1 [lines]
  (let [lines-transpose (map #(apply str %) (apply map vector lines))
        diagonals (all-diagonals lines)]
    (->> (concat lines lines-transpose diagonals)
         (mapv count-xmas)
         (reduce +))))

(defn x-mas? [box]
  (every? #{[\M \A \S] [\S \A \M]} box))

(defn all-boxes [lines]
  (let [len (count (first lines))
        left-corners (for [x (range (- len 2))
                           y (range (- len 2))]
                       [x y])
        pairs (for [[x y] left-corners
                    :let [add-to-point (fn [[dx dy]] [(+ x dx) (+ y dy)])]]
                [(mapv add-to-point [[0 0] [1 1] [2 2]])
                 (mapv add-to-point [[0 2] [1 1] [2 0]])])
        boxes (for [[left right] pairs]
                [(mapv #(get-in lines %) left)
                 (mapv #(get-in lines %) right)])]
    boxes))

(defn part2 [lines]
  (->> (all-boxes lines)
       (filter x-mas?)
       (count)))

(part1 (parse-input))
(part2 (parse-input))
