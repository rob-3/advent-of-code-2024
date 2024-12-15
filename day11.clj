(require '[clj-async-profiler.core :as prof])

(set! *warn-on-reflection* true)

(defn transform [stone]
  (cond
    (= stone "0") ["1"]
    (even? (count stone)) (let [[left right] (split-at (quot (count stone) 2) stone)
                                left (-> (apply str left) parse-long str)
                                right (-> (apply str right) parse-long str)]
                            [left right])
    :else (-> stone parse-long (* 2024) str vector)))

(comment
  (transform "0")
  (transform "124234")
  (transform "12234"))

(defn parse-input []
  (->> (slurp "input11.txt")
       (re-seq #"\d+")))

(defn part1 [input]
  (-> (iterate #(mapcat transform %) input)
      (nth 35)
      count))

(comment
  (prof/profile (part1 (parse-input)))
  (prof/profile (part2 (parse-input)))
  (prof/serve-ui 8080))

(defn count-num [n]
  (loop [i 1
         n n]
    (let [x (quot n 10)]
      (if (= x 0)
        i
        (recur (inc i) x)))))

(defn split-num [n ^java.util.ArrayList out]
  (let [digits-count (count-num n)]
    (loop [i 0
           n n
           right 0]
      (if (= i (quot digits-count 2))
         (doto out
           (.add n)
           (.add right))
         (recur (inc i) (quot n 10) (long (+ right (* (Math/pow 10 i) (rem n 10)))))))))

(comment
  (split-num 1234 (java.util.ArrayList.)))

(defn transform' [stone ^java.util.ArrayList out]
  (cond
    (= stone 0) (.add out 1)
    (even? (count-num stone)) (split-num stone out)
    :else (->> stone (* 2024) (.add out))))

(defn part2 [input]
  (loop [stones (java.util.ArrayList. (mapv parse-long input))
         i 0]
    (println i)
    (if (= 35 i)
      (.size stones)
      (let [stones' (java.util.ArrayList.)]
        (dotimes [i (.size stones)]
          (transform' (.get stones i) stones'))
        (recur stones' (inc i))))))
