(def input 
  (slurp "input.txt"))

(def starting-position 
  {:orientation :north
   :pos {:x 0 :y 0}})

(defn distance
  [{pos :pos}]
  (let [{:keys [x y]} pos]
    (+ (Math/abs x) (Math/abs y))))

(defn input->rel-moves
  "Converts input to moves, which are 2 element vectors containing
  the direction to turn and distance to travel"
  [input]
  (map (fn [[dir & dist]] 
         [dir (Integer/parseInt (apply str dist))]) 
       (clojure.string/split input #", ")))

(defn turn
  [orientation direction]
  (orientation (cond 
                 (= \R direction) {:north :east
                                   :east :south
                                   :south :west
                                   :west :north}
                 (= \L direction) {:north :west
                                   :west :south
                                   :south :east
                                   :east :north})))

(defn move-abs
  [cur-pos abs-move]
  (let [[orientation dist] abs-move
        pos (:pos cur-pos)
        next-pos (cond
                   (= orientation :north) (update pos :y + dist)
                   (= orientation :south) (update pos :y - dist)
                   (= orientation :east) (update pos :x + dist)
                   (= orientation :west) (update pos :x - dist))]
    {:orientation orientation :pos next-pos}))

(defn rel->abs-move
  [cur-pos rel-move]
  (let [[direction dist] rel-move
        {:keys [orientation]} cur-pos
        move-orientation (turn orientation direction)]
      [move-orientation dist]))

(defn move-rel 
  [position rel-move]
  (move-abs position (rel->abs-move position rel-move)))
     
(defn final-pos
  "Get final position after following a set of moves"
  [rel-moves]
  (reduce move-rel starting-position rel-moves))

(println "The answer to part1 is"
         (-> input
             input->rel-moves
             final-pos
             distance))
; 300

(defn first-repeated-visit
  "Gets the first position that was visited twice"
  ([abs-moves cur-pos visited]
   (if (seq abs-moves)
     (let [[next-move & rest-moves] abs-moves
           next-pos (move-abs cur-pos next-move)
           next-loc (:pos next-pos)]
       (if (contains? visited next-loc)
         next-pos
         (recur rest-moves next-pos (conj visited next-loc))))
      nil))
  ([moves cur-pos] (first-repeated-visit moves cur-pos #{(:pos cur-pos)}))
  ([moves] (first-repeated-visit moves starting-position)))

(defn rel->abs-moves
  "Converts seq of relative moves to a seq of abs moves"
  ([rel-moves initial-orientation]
   (first (reduce (fn [[abs-moves orientation] [direction dist]]
                    (let [next-orientation (turn orientation direction)
                          abs-move [next-orientation dist]]
                      [(conj abs-moves abs-move) next-orientation]))
                  [[] initial-orientation]
                  rel-moves)))
  ([rel-moves] (rel->abs-moves rel-moves :north)))

(defn to-unit-moves
  [abs-moves]
  (mapcat (fn [[orientation dist]]
            (repeat dist [orientation 1]))
          abs-moves))

(println "The answer to part 2 is"
         (-> input
             input->rel-moves
             rel->abs-moves
             to-unit-moves
             first-repeated-visit
             distance))
; 159