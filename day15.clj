(require '[clojure.string :as str])

(defn parse-input []
  (let [[grid moves] (->> (str/split (slurp "input15.txt") #"\n")
                          (split-with (complement #{""}))
                          (mapv vec))]
    {:grid (mapv vec grid) :moves (str/join "" moves)}))

(def move->direction
  {\< :left
   \^ :up
   \> :right
   \v :down})

(defn move-coord [[y x] move]
  (case move
    :left [y (dec x)]
    :up [(dec y) x]
    :right [y (inc x)]
    :down [(inc y) x]))

(defn move-item [grid cur dir]
  (let [current-item (get-in grid cur)
        dest (move-coord cur dir)
        dest-item (get-in grid dest)]
    (case dest-item
      \# {:success false :grid grid}
      \. {:success true
          :grid (-> grid
                    (assoc-in dest current-item)
                    (assoc-in cur \.))}
      \O (let [{:keys [success grid]} (move-item grid dest dir)]
            (if success
              {:success true
               :grid (-> grid 
                         (assoc-in dest current-item)
                         (assoc-in cur \.))}
              {:success false :grid grid})))))

(defn coords [grid]
  (for [x (range (count grid))
        y (range (count (get grid 0)))]
      [y x]))

(defn simulate [grid moves]
  (let [robot (->> (coords grid)
                   (some #(when (= \@ (get-in grid %)) %)))]
    (loop [[move & moves] moves
           grid grid
           robot robot]
      (if (nil? move)
        grid
        (let [dir (move->direction move)
              {grid' :grid success :success} (move-item grid robot dir)
              robot' (if success (move-coord robot dir) robot)]
          (recur moves grid' robot'))))))
            
(defn part1 [& {:keys [grid moves]}]
  (let [final-grid (simulate grid moves)]
    (->> (for [[y x] (coords final-grid)]
          (if (= \O (get-in final-grid [y x]))
            (+ (* 100 y) x)
            0))
         (reduce +))))

(comment
  (part1 (parse-input)))
