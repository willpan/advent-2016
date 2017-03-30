(def input (slurp "input.txt"))

; (def keypad
;   {:width 3
;    :height 3
;    :pos 4})

; (defn pos-to-number
;   "Convert keypad position to keypad number"
;   [pos]
;   (+ pos 1))

; (defn move-left
;   [{width :width, height :height, pos :pos, :as keypad}]
;   (let [row (quot pos width)
;         col (mod pos height)]
;     (if (> col 0)
;       (update keypad :pos dec)
;       keypad)))

; (defn move-right
;   [{width :width, height :height, pos :pos, :as keypad}]
;   (let [row (quot pos width)
;         col (mod pos height)]
;     (if (< (+ col 1) width)
;       (update keypad :pos inc)
;       keypad)))
  
; (defn move-up
;   [{width :width, height :height, pos :pos, :as keypad}]
;   (let [row (quot pos width)
;         col (mod pos height)]
;     (if (> row 0)
;       (update keypad :pos - width)
;       keypad)))

; (defn move-down
;   [{width :width, height :height, pos :pos, :as keypad}]
;   (let [row (quot pos width)
;         col (mod pos height)]
;     (if (< (+ row 1) height)
;       (update keypad :pos + width)
;       keypad)))

(def keypad 4)

(defn pos-to-number
  [keypad]
  (format "%X" keypad))

(defn move-left
  [keypad]
  (if (#{3 4 6 7 8 9 11 12} keypad)
    (dec keypad)
    keypad))

(defn move-right
  [keypad]
  (if (#{2 3 5 6 7 8 10 11} keypad)
    (inc keypad)
    keypad))

(defn move-up
  [keypad]
  (get {3 1, 6 2, 7 3, 8 4, 10 6, 11 7, 12 8, 13 11} keypad keypad))

(defn move-down
  [keypad]
  (get {1 3, 2 6, 3 7, 4 8, 6 10, 7 11, 8 12, 11 13} keypad keypad))

(def dir-to-mover
  {\U move-up
   \D move-down 
   \L move-left
   \R move-right})

(defn find-key
  "Takes an instruction line and a current keypad state and returns the
  ending keypad state. If no keypad state is specified, the default 3x3
  keypad is assumed."
  ([line keypad]
   (reduce (fn [prev-keypad move-fn]
             (move-fn prev-keypad))
           keypad
           (map dir-to-mover line)))
  ([line] (find-key line keypad)))

(defn find-passcode
  "Takes a sequence of instruction lines and a starting keypad state and
  returns a vector of the numbers to press"
  ([instructions keypad]
   (first (reduce (fn [[code prev-keypad] line]
                    (let [next-keypad (find-key line prev-keypad)
                          next-key (pos-to-number next-keypad)]
                      [(conj code next-key) next-keypad]))
                  [[] keypad]
                  instructions))))

(println "The answer to part b is:"
         (-> input
             (clojure.string/split #"\n")
             (find-passcode keypad)))
; part a: 12578
; part b: 516DD