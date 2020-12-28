(ns advent-of-code-2020.day-25
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def input
  (->> "day-25.txt"
       (io/resource)
       (slurp)
       (string/split-lines)
       (map read-string)
       (apply (fn [fst snd] {:card fst, :door snd}))
       )
  )

(defn transform [subject-number]
  (iterate (fn [n] (mod (* n subject-number) 20201227)) 1)
  )

(defn transform-n [subject-number loop-size]
  (nth (transform subject-number) loop-size)
  )

(defn find-loop-size [subject-number target]
  (count
    (take-while
      #(not= target %)
      (transform subject-number)
      )
    )
  )

(let [card-loop-size (find-loop-size 7 (:card input))
      encryption-key (transform-n (:door input) card-loop-size)]
  encryption-key
  )
