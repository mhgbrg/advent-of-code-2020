(ns advent-of-code-2020.day-06
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(def input
  (map string/split-lines
       (-> "day-06.txt"
           (io/resource)
           (slurp)
           (string/split #"\n\n")
           )
       )
  )

; part 1
(defn answered-questions-1 [answers]
  (apply set/union (map set answers))
  )

(apply + (map (comp count answered-questions-1) input))

; part 2
(defn answered-questions-2 [answers]
  (apply set/intersection (map set answers))
  )

(apply + (map (comp count answered-questions-2) input))
