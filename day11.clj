(defn transform [stone]
  (cond
    (= stone "0") ["1"]
    (even? (count stone)) (let [[left right] (split-at (quot (count stone) 2) stone)
                                 left (-> (apply str left) bigint str)
                                 right (-> (apply str right) bigint str)]
                            [left right])
    :else (-> stone bigint (* 2024) str vector)))

(comment
  (transform "0")
  (transform "124234")
  (transform "12234"))

(defn parse-input []
  (->> (slurp "input11.txt")
       (re-seq #"\d+")))

(defn part1 [input]
  (-> (iterate #(mapcat transform %) input)
      (nth 25)
      count))

(comment
  (part1 (parse-input)))
