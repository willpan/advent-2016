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

(defn move
  [cur-pos next-move]
  (let [[direction dist] next-move
        {:keys [orientation pos]} cur-pos
        next-orientation (turn (:orientation cur-pos) direction)
        next-pos (cond
                   (= next-orientation :north) (update pos :y + dist)
                   (= next-orientation :south) (update pos :y - dist)
                   (= next-orientation :east) (update pos :x + dist)
                   (= next-orientation :west) (update pos :x - dist))]
    {:orientation next-orientation :pos next-pos}))
     
(defn final-pos
  "Get final position after following a set of moves"
  [rel-moves]
  (reduce move starting-position rel-moves))

(def answer-to-part1
  (-> input
      input->rel-moves
      final-pos
      distance))
; 300
