(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn parse-input []
  (let [[rules input] (as-> (slurp "input5.txt") $
                        (str/split $ #"\n\n")
                        (mapv (fn [x]
                                (->> (str/split x #"\n")
                                     (mapv #(->> (re-seq #"\d+" %)
                                                 (mapv parse-long)))))
                              $))]
    {:rules rules
     :input input}))

(defn middle [seq]
  (nth seq (quot (count seq) 2)))

(defn valid-order? [order rules]
  (let [n->illegal-set (reduce (fn [m [l r]]
                                 (update m r (fnil conj #{}) l))
                               {}
                               rules)]
    (loop [xs order
           illegals #{}]
      (let [x (first xs)]
        (cond
          (nil? x) true
          (contains? illegals x) false
          :else (recur
                 (rest xs)
                 (set/union illegals (n->illegal-set x))))))))

(defn part1 [& {:keys [rules input]}]
  (reduce + (for [i input]
              (if (valid-order? i rules)
                (middle i)
                0))))

(defn topo-sort
  ([node->nodes] (loop [nodes (keys node->nodes)
                        sorted '()
                        visited? #{}]
                   (if-let [node (first nodes)]
                     (let [[sorted visited?] (topo-sort node->nodes node sorted visited?)]
                       (recur (rest nodes) sorted visited?))
                     sorted)))
  ([node->nodes node sorted visited?]
   (if (visited? node)
     [sorted visited?]
     (if-let [deeper-nodes (node->nodes node)]
       (loop [deeper-nodes deeper-nodes
              sorted sorted
              visited? visited?]
         (if-let [n (first deeper-nodes)]
           (let [[sorted visited?] (topo-sort node->nodes n sorted (conj visited? node))]
             (recur (rest deeper-nodes) sorted visited?))
           [(conj sorted node) (conj visited? node)]))
       [(conj sorted node) (conj visited? node)]))))

(defn sort-using [s order]
  (let [s (set s)]
    (loop [sorted []
           order order]
      (if (empty? order)
        sorted
        (let [x (first order)]
          (recur (if (s x) (conj sorted x) sorted) (rest order)))))))

(defn part2 [& {:keys [rules input]}]
  (let [node->nodes (reduce (fn [acc [l r]]
                              (update acc l (fnil conj []) r)) {} rules)
        order (topo-sort node->nodes)]
    (for [i input]
      (if (valid-order? i rules)
        0
        (-> (sort-using i order)
            (middle))))))

(comment
  (part1 (parse-input))
  (part2 (parse-input))
  (topo-sort {22 [0 100 17] 0 [100] 17 [100]})
  (topo-sort {14 [15] 15 []}))
