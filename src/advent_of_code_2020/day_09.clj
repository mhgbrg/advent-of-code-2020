(ns advent-of-code-2020.day-09
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import (clojure.lang PersistentQueue)))

(def input
  (->> "day-09.txt"
       (io/resource)
       (slurp)
       (string/split-lines)
       (map read-string)
       )
  )

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

;part 1
(let [init-preamble (take 25 input)
      init-rest (drop 25 input)]
  (loop [preamble init-preamble
         rest init-rest]
    (let [n (first rest)]
      (if (valid? preamble n)
        (recur
          (drop 1 (concat preamble [n]))
          (drop 1 rest)
          )
        n
        )
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
        (< sum target) (recur (concat terms [(first remaining)]) (drop 1 remaining))
        ; target too low, remove an old element from terms without advancing remaining
        (> sum target) (recur (drop 1 terms) remaining)
        )
      )
    )
  )

(let [sum (find-terms input 104054607)]
  (+ (apply min sum) (apply max sum))
  )

