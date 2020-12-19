(ns advent-of-code-2020.day-08
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-line [line]
  (let [[op arg] (string/split line #" ")]
    [(keyword op) (read-string arg)]
    )
  )

(def input
  (->> "day-08.txt"
       (io/resource)
       (slurp)
       (string/split-lines)
       (map parse-line)
       (vec)
       )
  )

(defn execute [get-op]
  (loop [i 0
         acc 0
         executed-ops #{}]
    ;(prn i acc executed-ops)
    (if (contains? executed-ops i)
      [:loop acc]
      (let [[op arg] (get-op i)
            executed-ops' (conj executed-ops i)]
        (case op
          :acc (recur (inc i) (+ acc arg) executed-ops')
          :jmp (recur (+ i arg) acc executed-ops')
          :nop (recur (inc i) acc executed-ops')
          [:term acc]
          )
        )
      )
    )
  )

;part 1
(execute (fn [i] (get input i)))

;part 2
(defn jmp-nop-indices [ops]
  (keep-indexed
    (fn [i [op _]] (when (contains? #{:jmp :nop} op) i))
    ops
    )
  )

(defn get-op [ops swap-i i]
  (let [[op arg] (get ops i)]
    (if (= i swap-i)
      (case op
        :jmp [:nop arg]
        :nop [:jmp arg]
        )
      [op arg]
      )
    )
  )

(->> (for [swap-i (jmp-nop-indices input)] (execute (fn [i] (get-op input swap-i i))))
     (filter #(= (first %) :term))
     (first)
     )
