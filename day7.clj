(require '[clojure.string :as str])

(defn parse-input []
  (let [lines (-> (slurp "input7.txt")
                  (str/split #"\n"))
        inputs (mapv #(->> %
                           (re-seq #"\d+")
                           (mapv parse-long)) lines)]
    inputs))

(defn part1 [inputs]
  (let [possibles (mapv (fn [[total x & nums]]
                          (loop [nums nums
                                 superpositions #{x}]
                            (if-let [n (first nums)]
                              (let [times (mapv * superpositions (repeat n))
                                    plus (mapv + superpositions (repeat n))]
                                (recur (rest nums) (into #{} (concat times plus))))
                              (contains? superpositions total))))
                        inputs)]
    (reduce +
            (for [[p sum] (map vector possibles (mapv first inputs))]
              (if p sum 0)))))

(defn part2 [inputs]
  (let [possibles (mapv (fn [[total x & nums]]
                          (loop [nums nums
                                 superpositions #{x}]
                            (if-let [n (first nums)]
                              (let [times (mapv * superpositions (repeat n))
                                    plus (mapv + superpositions (repeat n))
                                    || (mapv (comp bigint str) (mapv str superpositions) (repeat (str n)))]
                                (recur (rest nums) (into #{} (concat times plus ||))))
                              (contains? superpositions total))))
                        inputs)]
    (reduce +
            (for [[p sum] (map vector possibles (mapv first inputs))]
              (if p sum 0)))))

(comment
  (part1 (parse-input))
  (part2 (parse-input)))
