(require '[clojure.set :as set])

(defn parse-input []
  (->> (slurp "input12.txt")
       (re-seq #"\S+")
       vec))

(defn adjacent [[y x]]
  [[(inc y) x]
   [y (inc x)]
   [(dec y) x]
   [y (dec x)]])

(defn grid-neighbors [grid coord]
  (->> (adjacent coord)
       (filterv #(get-in grid %))))

(comment
  (def grid (parse-input))
  (grid-neighbors grid [0 0])
  (adjacent [0 0]))

;; it's floodfill
(defn regionize [& {:keys [node grid]}]
  (let [plant-type (get-in grid node)]
    (loop [to-visit (list node)
           region #{}
           visited #{}]
      (if (empty? to-visit)
        {:region region :visited visited}
        (let [[node & to-visit'] to-visit]
          (recur (into to-visit' (->> node
                                      (grid-neighbors grid)
                                      (remove visited)
                                      (filterv #(= plant-type (get-in grid %)))))
                 (conj region node)
                 (conj visited node)))))))

(comment
  (regionize :node [0 0]
             :grid grid))

(defn perimeter [region]
  (->> (mapv #(->> %
                   adjacent
                   (remove region)
                   count) 
            region)
       (reduce +)))

(comment
  (perimeter #{[0 0]})
  (perimeter #{[0 0] [0 1]})
  (perimeter #{[0 0] [0 1] [1 0]}))

(defn price [region]
  (let [area (count region)
        peri (perimeter region)]
    (* area peri)))

(defn part1 [grid]
  (let [coords (for [y (-> grid count range)
                     x (-> grid first count range)]
                 [y x])
        regions (loop [visited? #{}
                       coords coords
                       regions []]
                  (let [[coord & coords'] coords]
                    (cond
                      (nil? coord) regions
                      (visited? coord) (recur visited? coords' regions)
                      :else (let [{:keys [region visited]} (regionize :node coord :grid grid)]
                              (recur (set/union visited? visited) coords' (conj regions region))))))]
    (->> (mapv price regions)
         (reduce +))))

(comment
  (part1 (parse-input)))
