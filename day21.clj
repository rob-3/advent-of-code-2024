(defn parse-input []
  (->> (slurp "input21.txt")
       (re-seq #"(.+)\n")
       (mapv second)
       (mapv (juxt vec
                   #(->> %
                         (re-find #"\d+")
                         (parse-long))))))

(def shortest-path-directional
  ;; [from to] => path
  {[\A \^] [:left]
   [\A \>] [:down]
   [\A \<] [:down :left :left]
   [\A \v] [:down :left]

   [\^ \A] [:right]
   [\^ \>] [:right :down]
   [\^ \v] [:down]
   [\^ \<] [:down :left]

   [\> \A] [:up]
   [\> \v] [:left]
   [\> \<] [:left :left]
   [\> \^] [:left :up]

   [\v \A] [:right :up]
   [\v \>] [:right]
   [\v \<] [:left]
   [\v \^] [:right :up]

   [\< \A] [:right :right :up]
   [\< \v] [:right]
   [\< \>] [:right :right]
   [\< \^] [:right :up]

   [\< \<] []
   [\> \>] []
   [\^ \^] []
   [\v \v] []})

(def coords
  {\7 [0 3] \8 [1 3] \9 [2 3]
   \4 [0 2] \5 [1 2] \6 [2 2]
   \1 [0 1] \2 [1 1] \3 [2 1]
   #_"    " \0 [1 0] \A [2 0]})

(defn naive-shortest-path [start-coord finish-coord]
  ())

(defn shortest-path-numeric [start finish facing]
  (let [start-coords (coords start)
        finish-coords (coords finish)]))

(defn press [state button])

(defn shortest-presses [buttons]
  (loop [state {:dir-1 \A :dir-2 \A :numeric \A :keypresses []}
         [button & remaining] buttons]
    (-> state
        (press button)
        (recur remaining))))

(defn part1 [input]
  input)

(comment
  (shortest-presses [\0 \2 \9 \A])
  (part1 (parse-input)))
