(require '[clojure.data.priority-map :refer [priority-map-keyfn]])

(defn parse-input []
  (->> (slurp "input16.txt")
       (re-seq #"(.+)\n")
       (mapv second)
       (mapv vec)))

(defn find-char [grid c]
  (let [width (count (get grid 0))]
    (->> (range)
         (map #(let [y (quot % width)
                     x (mod % width)]
                 [[y x] (get-in grid [y x])]))
         (filter (comp #{c} second))
         first
         first)))

(defn find-start [grid]
  (find-char grid \S))

(defn find-end [grid]
  (find-char grid \E))

(defn reconstruct-path [from finish work-queue]
  (loop [from from
         path (list finish)]
     (if from
       (recur (get-in work-queue [from :from]) (conj path from))
       path)))

(defn a*
  "An A* implementation
   neighbors-fn :: node -> {:node node :cost +int}[]
   heuristic-fn :: node, node -> float
   start :: node
   finish :: node
   assumes no negative weights or heuristic-fn return values"
  [& {:keys [heuristic-fn start finish]
      neighbors-of :neighbors-fn}]
  (if (= start finish) {:cost 0 :path (list start)}
      (loop [work-queue (priority-map-keyfn :heuristic
                                            start {:heuristic ##-Inf :cost 0})
             best-paths #{}
             best-cost ##Inf]
        (pprint best-paths)
        (pprint best-cost)
        (pprint work-queue)
        (let [current-data (first work-queue)
              [node {:keys [cost from heuristic]}] current-data]
          (cond
            (> cost best-cost) {:cost best-cost
                                :paths best-paths}
            (= node finish) (recur (update work-queue node assoc :heuristic ##Inf)
                                   (conj best-paths (reconstruct-path from finish work-queue))
                                   cost)
            (= ##Inf heuristic) :no-path
            :else (let [neighbors (neighbors-of node)
                        neighbors-data (for [{neighbor :node neighbor-cost :cost} neighbors
                                             :let [total-cost (+ cost neighbor-cost)
                                                   current-cost (get-in work-queue [neighbor :cost])]
                                             :when (<= total-cost (or current-cost ##Inf))]
                                         [neighbor
                                          {:heuristic (+ total-cost (heuristic-fn finish neighbor))
                                           :cost total-cost
                                           :from node}])
                        work-queue (as-> work-queue $
                                       (reduce (fn [q [k v]] 
                                                 (if (< (:cost v) (get-in q [k :cost] ##Inf))
                                                   (assoc q k v)
                                                   (update-in q [k :from] conj (:from v))))
                                               $ neighbors-data)
                                       (update $ node assoc :heuristic ##Inf))]
                    (recur work-queue best-paths best-cost)))))))

(def turn-cost
  {[:north :north] 0
   [:east :east] 0
   [:south :south] 0
   [:west :west] 0

   [:north :south] 2000
   [:east :west] 2000
   [:south :north] 2000
   [:west :east] 2000

   [:north :east] 1000
   [:north :west] 1000
   [:east :north] 1000
   [:east :south] 1000
   [:south :east] 1000
   [:south :west] 1000
   [:west :north] 1000
   [:west :south] 1000})

(defn reindeer-distance [[y1 x1 dir1] [y2 x2 dir2]]
  (let [dy (abs (- y2 y1))
        dx (abs (- x2 x1))]
    (-> (cond
          (zero? dy) dx
          (zero? dx) dy
          :else (+ dy dx (turn-cost [dir1 dir2]))))))

(def dir->vec
  {:north [-1 0]
   :east [0 1]
   :south [1 0]
   :west [0 -1]})

(def turn-clockwise
  {:north :east
   :east :south
   :south :west
   :west :north})

(def turn-counterclockwise
  {:north :west
   :west :south
   :south :east
   :east :north})

(defn directional-neighbors [& {grid :grid
                                [fy fx fdir] :finish
                                [y x dir] :node}]
  (if (= [fy fx] [y x])
    [{:node [fy fx fdir] :cost 0}]
    (let [[y' x'] (mapv + [y x] (dir->vec dir))]
      (->> [{:node [y' x' dir] :cost 1}
            {:node [y x (turn-clockwise dir)] :cost 1000}
            {:node [y x (turn-counterclockwise dir)] :cost 1000}]
           (filterv (fn [{[y x] :node}]
                      (-> (get-in grid [y x])
                          #{\. \S \E})))))))

(defn part1 [grid]
  (let [start (conj (find-start grid) :east)
        end (conj (find-end grid) :north)]
    (-> (a* :heuristic-fn reindeer-distance
            :start start
            :finish end
            :neighbors-fn #(directional-neighbors :grid grid
                                                  :finish end
                                                  :node %))
        :cost)))

(defn part2 [grid]
  (let [start (conj (find-start grid) :east)
        end (conj (find-end grid) :north)
        coords-of #(subvec % 0 2)]
    (->> (a* :heuristic-fn reindeer-distance
             :start start
             :finish end
             :neighbors-fn #(directional-neighbors :grid grid
                                                   :finish end
                                                   :node %))
         :paths
         ;(reduce concat)
         ;(mapv coords-of)
         ;set
         count)))

(comment
  (def grid (parse-input))
  (a* :heuristic-fn reindeer-distance
      :start [13 1 :east]
      :finish [1 13 :north]
      :neighbors-fn #(directional-neighbors :grid grid
                                            :finish [1 13 :north]
                                            :node %))
  (reindeer-distance [1 1 :north] [2 5 :east])
  (directional-neighbors grid [13 1 :north])
  (part1 (parse-input))
  (part2 (parse-input)))
