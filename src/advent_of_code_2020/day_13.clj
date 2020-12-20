(ns advent-of-code-2020.day-13
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.math.numeric-tower :as math]))

(def input
  (let [[fst snd] (->> "day-13.txt"
                       (io/resource)
                       (slurp)
                       (string/split-lines))]
    [(read-string fst)
     (->> (string/split snd #",")
          (map (fn [x] (case x "x" :x (read-string x))))
          )
     ]
    )
  )

;part 1
(defn earliest [target line]
  (* (int (math/ceil (/ target line))) line)
  )

(defn find-earliest [target lines]
  (first
    (sort-by
      second
      (for [line lines] [line (earliest target line)])
      )
    )
  )

(let [target (first input)
      lines (filter #(not= :x %) (second input))
      [line timestamp] (find-earliest target lines)]
  (* line (- timestamp target))
  )

;part 2
(defn extended-gcd [a b]
  (loop [[old-r r] [a b]
         [old-s s] [1 0]
         [old-t t] [0 1]]
    (if (= 0 r)
      {:x   old-s
       :y   old-t
       :gcd old-r
       :t   t
       :s   s}
      (let [q (long (/ old-r r))]
        (recur
          [r (- old-r (* q r))]
          [s (- old-s (* q s))]
          [t (- old-t (* q t))])
        )
      )
    )
  )

(defn term [M r m]
  (let [Mi (/ M m)
        xi (:x (extended-gcd Mi m))]
    (* r Mi xi)
    )
  )

(defn chinese [rests mods]
  (let [M (apply * mods)
        terms (map (partial term M) rests mods)
        sum (apply + terms)]
    (mod sum M)
    )
  )

(let [[rests mods] (->> (map vector (range) (second input))
                        (filter #(not= :x (second %)))
                        (map (fn [[i m]] [(- m i) m]))
                        (apply map vector))]
  (chinese rests mods)
  )

