(ns advent-of-code-2020.day-03
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def input
  (mapv
    (fn [row] (mapv {\# true \. false} row))
    (string/split-lines (slurp (io/resource "day-03.txt")))
  )
)

(defn tree? [m [x y]]
  (let [x-bound (count (first m))
        xi (mod x x-bound)]
    (get-in m [y xi])
  )
)

; recursive solution
(defn traverse [m x y dx dy]
  (let [y-bound (count m)]
    (if (< y y-bound)
      (+
        (if (true? (tree? m [x y])) 1 0)
        (traverse m (+ x dx) (+ y dy) dx dy)
      )
      0
    )
  )
)

; part 1
(traverse input 0 0 3 1)

; part 2
(*
  (traverse input 0 0 1 1)
  (traverse input 0 0 3 1)
  (traverse input 0 0 5 1)
  (traverse input 0 0 7 1)
  (traverse input 0 0 1 2)
)

; iterative solution
(defn move [[dx dy] [x y]]
  [(+ x dx)
   (+ y dy)
  ]
)

(defn get-positions [m [dx dy]]
  (let [y-bound (count m)]
    (take-while
      (fn [[_ y]] (< y y-bound))
      (iterate (partial move [dx dy]) [0 0])
    )
  )
)

(defn count-trees [m [dx dy]]
  (->> (get-positions m [dx dy])
       (map (partial tree? m))
       (filter true?)
       (count)
  )
)

; part 1
(count-trees input [3 1])

; part 2
(let [slopes '([1 1] [3 1] [5 1] [7 1] [1 2])]
  (->> slopes
       (map (partial count-trees input))
       (reduce *)
  )
)
