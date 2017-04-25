(import 'java.security.MessageDigest
        'java.math.BigInteger)

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
      (format "%032x" (BigInteger. 1 raw))))

(def input "abbhdwsy")

(comment
  (def solution-a
    (->> (range)
        (map (comp md5 #(str input %)))                    ; calculate md5 hashes of input with incrementing counter
        (filter #(clojure.string/starts-with? % "00000"))  ; find ones that start with 00000
        (map #(nth % 5))                                   ; get first non-zero character
        (take 8)                                           ; take first 8
        (apply str)))

  (println "The solution to part a is:" solution-a))
  ; The solution to part a is: 801b56a7

(defn between [lower upper x]
  (and (<= (int lower) (int x))
       (>= (int upper) (int x))))

(def solution-b
  (->> (range)
       (map (comp md5 #(str input %)))                   ; calculate md5 hashes of input
       (filter #(clojure.string/starts-with? % "00000")) ; find one that start with 00000
       (map (juxt #(nth % 5) #(nth % 6)))                ; get the first two digist after the 0s
       (filter #(between \0 \7 (first %)))
       (reduce (fn [codes [key val]]
                 (if (contains? codes key)
                   codes
                   (let [new-codes (assoc codes key val)]
                     (if (>= (count new-codes) 8)
                       (reduced new-codes)
                       new-codes))))
               {})))

(println "The solution to part b is:"
         (->> (range 48 56)
              (map (comp solution-b char))
              (apply str)))
; The solution to part b is: 424a0197
