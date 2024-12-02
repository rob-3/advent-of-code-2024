(require '[clojure.string :as str])

(defn parse-input []
  (let [lines (-> (slurp "input2.txt")
                  (str/split #"\n"))]
    (for [line lines]
      (map parse-long (re-seq #"\d+" line)))))

(defn decreasing? [report]
  (->> report
       (partition 2 1)
       (map #(apply - %))
       (every? #(<= 1 % 3))))

(defn increasing? [report]
  (decreasing? (map - report)))

(defn part1 [reports]
  (->> reports
       (filter #(or (decreasing? %) (increasing? %)))
       (count)))

(defn decreasing-except-one? [report]
  (let [except-first? (decreasing? (rest report))
        otherwise-decreasing?
        (loop [report report
               damped? false]
          (if (= 1 (count report))
            true
            (let [[a b] report]
              (if (<= 1 (- a b) 3)
                (recur (rest report) damped?)
                (if damped?
                  false
                  (recur (conj (nthrest report 2) a) true))))))]
    (or except-first? otherwise-decreasing?)))

(defn valid-report? [report]
  (let [high-to-low? (decreasing-except-one? report)
        low-to-high? (decreasing-except-one? (map - report))]
    (or high-to-low? low-to-high?)))

(defn part2 [reports]
  (->> reports
       (filter valid-report?)
       (count)))

(part1 (parse-input))
(part2 (parse-input))
