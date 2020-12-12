(ns advent-of-code-2020.day-05
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def example-input "FBFBBFFRLR")

(def input
  (-> "day-05.txt"
      (io/resource)
      (slurp)
      (string/split-lines)
      )
  )

(def binary-mapping {\F \0 \B \1 \L \0 \R \1})

(defn parse-as-binary [val]
  (let [binary (map binary-mapping val)]
    (Integer/parseInt (apply str binary) 2)
    )
  )

(defn parse-seat [seat]
  (let [[row col] (partition-all 7 seat)]
    {:row (parse-as-binary row)
     :col (parse-as-binary col)
     }
    )
  )

(defn seat-id [{:keys [row col]}]
  (+ (* row 8) col)
  )

; part 1
(apply max (map (comp seat-id parse-seat) input))

; part 2
(let [ids (map (comp seat-id parse-seat) input)
      min (apply min ids)
      max (apply max ids)
      lookup (set ids)]
  (some
    (fn [i] (when (not (contains? lookup i)) i))
    (range min max)
    )
  )
