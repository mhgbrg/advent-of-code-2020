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
    #{n}
    (for [x preamble
          y preamble
          :when (< x y)]
        (+ x y)
      )
    )
  )

(->> (for [part (partition (inc preamble-size) 1 input)
           :let [preamble (butlast part)
                 n (last part)]]
       (when-not (valid? preamble n) n)
       )
     (filter (comp not nil?))
     (first)
     )

;part 2
(defn find-terms [nums target]
  (loop [terms ()
         remaining nums]
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

