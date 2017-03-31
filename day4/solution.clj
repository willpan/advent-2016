(def input
  (-> (slurp "input.txt")
      clojure.string/split-lines))

(def example
  "zsxyfgqj-hqfxxnknji-idj-xytwflj-359[jxfin]")

(def data-pattern #"([a-z-]+)-(\d+)\[(\w+)\]")

(defn parse-data
  "Given a single item as string, returns map of the room-name, sector-id,
  and checksum"
  [data]
  (let [[_ name sector-id checksum] (re-find data-pattern data)]
    {:room-name name
     :sector-id (Integer/parseInt sector-id)
     :checksum checksum}))
; (parse-data example) => {:room-name "zsxyfgqj-hqfxxnknji-idj-xytwflj",
;                          :sector-id 359, :checksum "jxfin" }

(defn- item-freq-comparator
  "Comparator for [item freq] tuples for sorting on frequency descending and
  item ascending"
  [[char-a freq-a] [char-b freq-b]]
  (let [freq-same? (compare freq-b freq-a)]
    (if (= 0 freq-same?) (compare char-a char-b) freq-same?)))

(defn calc-checksum
  "Calculate the checksum of a room name, which is the 5 most frequent
  characters in the name, ordered by frequency, with ties broken by
  alphabetical ordering."
  [name]
  (->> name
       (filter #(Character/isLetter %)) ; remove dashes
       frequencies
       (sort item-freq-comparator)
       (map first)
       (take 5)
       (apply str)))
; (calc-checksum example) => "jxfin"


(def solution-part-1
  "Sum of the sector-ids of real rooms. Room is real if checksum of name
  matches the checksum in the encrypted entry"
  (->> input
       (map parse-data)
       (filter #(= (:checksum %) (calc-checksum (:room-name %))))
       (map :sector-id)
       (reduce +)))

(println "The solution to part a is:" solution-part-1)
; The solution to part a is: 173787