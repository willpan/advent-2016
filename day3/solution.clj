(defn parse-int-list
  "Parse a seq of integer strings into integers"
  [col]
  (map #(Integer/parseInt %) col))

(def input
  (->> (slurp "input.txt")
       (clojure.string/split-lines)
       (map clojure.string/trim)
       (map #(clojure.string/split % #"\s+"))
       (map parse-int-list)))

(defn valid-triangle?
  "Given three side lengths, check if they are valid side lengths for a triangle"
  [a b c]
  (let [[x y z] (sort [a b c])]
    (> (+ x y) z)))

(def possible-triangles
  (count (filter #(apply valid-triangle? %) input)))

(println "The solution to part a is:" possible-triangles)
; 993

(defn transpose
  "Given an n-by-n list, returns the transposed list, e.g. the first column
  is now the first row"
  [n coll]
  (map (fn [i]
         (map (fn [row] (nth row i)) coll))
       (range n)))

(def input-part-2
  (mapcat (fn [rows] (transpose 3 rows))
          (partition 3 input)))

(def possible-triangles-2
  (->> input-part-2
       (filter #(apply valid-triangle? %))
       count))

(println "The solution to part b is:" possible-triangles-2)
; 1849
