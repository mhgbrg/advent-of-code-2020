(ns advent-of-code-2020.day-09
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def input
  (->> "day-09.txt"
       (io/resource)
       (slurp)
       (string/split-lines)
       (map read-string)
       )
  )

(def preamble-size 25)

;part 1
(defn valid? [preamble n]
  (some
    true?
    (for [n1 preamble
          n2 preamble]
      (and
        (not= n1 n2)
        (= (+ n1 n2) n)
        )
      )
    )
  )

(loop [preamble (take preamble-size input)
       rest (drop preamble-size input)]
  (let [n (first rest)]
    (if (valid? preamble n)
      (recur
        (next (concat preamble [n]))
        (next rest)
        )
      n
      )
    )
  )

;part 2
(defn find-terms [list target]
  (loop [terms ()
         remaining list]
    (let [sum (apply + terms)]
      (cond
        ; target found, return terms
        (= sum target) terms
        ; target too low, add a new element to terms from remaining
        (< sum target) (recur (concat terms [(first remaining)]) (next remaining))
        ; target too low, remove an old element from terms without advancing remaining
        (> sum target) (recur (next terms) remaining)
        )
      )
    )
  )

(let [sum (find-terms input 104054607)]
  (+ (apply min sum) (apply max sum))
  )

