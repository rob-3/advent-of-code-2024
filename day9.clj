(defn parse-input []
  (->> (slurp "input9.txt")
       (re-seq #"\d")
       (mapv parse-long)))

(defn make-disk [xs]
  (loop [disk []
         i 0]
    (if-let [n (get xs i)]
      (if (even? i)
        (recur (into disk (repeat n (/ i 2))) (inc i))
        (recur (into disk (repeat n \.)) (inc i)))
      disk)))

(comment
  (make-disk [1 2 3 4 5]))

(defn part1 [input]
  (let [disk (make-disk input)]
    (loop [left 0
           right (dec (count disk))
           checksum 0]
      (let [l (nth disk left)
            r (nth disk right)]
        (cond
          (>= left right) checksum
          (int? l) (recur (inc left) right (+ checksum (* left l)))
          (int? r) (recur (inc left) (dec right) (+ checksum (* left r)))
          :else (recur left (dec right) checksum))))))

(defn find-free [disk size]
  (loop [i 1]
    (cond
      (>= i (count disk)) nil
      (-> (nth disk i) :size (>= size)) i
      :else (recur (+ 2 i)))))

(defn make-disk-2 [input]
  (map-indexed #(hash-map :id (quot %1 2)
                          :size %2
                          :type (if (even? %1) :file :free))
               input))
(comment
  (find-free (make-disk-2 [1 2 3 4 5]) 3))

(defn part2 [input]
  (let [disk (make-disk-2 input)
        size (count disk)]
    (loop [disk disk
           i (if (odd? size) (dec size) (- 2 size))]
      (let [{:keys [size type id]} (get disk i)]
        (cond
          (zero? i) disk
          (= type :free) (recur disk (dec i))
          :else ())))))

(comment
  (part1 (parse-input))
  (part2 [2 3 3 3 1 3 3 1 2 1 4 1 4 1 3 1 4 0 2]))
