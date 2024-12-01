(defn parse-input []
  (let [numbers (->> (slurp "input1.txt")
                     (re-seq #"\d+")
                     (map parse-long))
        left (take-nth 2 numbers)
        right (take-nth 2 (rest numbers))]
    [left right]))

(defn part1 []
  (let [[left right] (parse-input)
        [left-sorted right-sorted] (map sort [left right])
        differences (map (comp abs -) left-sorted right-sorted)]
    (reduce + differences)))

(defn part2 []
  (let [[left right] (parse-input)
        [left-freqs right-freqs] (map frequencies [left right])
        similarity-subscores (for [[n freq] left-freqs]
                               (* n freq (get right-freqs n 0)))]
    (reduce + similarity-subscores)))

(comment
  (part1)
  (part2))
