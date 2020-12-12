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

(defn parse-binary-str [val binary-mapping]
  (let [binary-str (apply str (replace binary-mapping val))]
    (Integer/parseInt binary-str 2)
    )
  )

(defn parse-row [row] (parse-binary-str row {\F \0 \B \1}))

(defn parse-col [col] (parse-binary-str col {\L \0 \R \1}))

(defn parse-seat [seat]
  (let [row (subs seat 0 7)
        col (subs seat 7)]
    {:row (parse-row row)
     :col (parse-col col)
     }
    )
  )

(defn seat-id [{:keys [row col]}]
  (+ (* row 8) col)
  )

; part 1
(reduce max (map (comp seat-id parse-seat) input))

; part 2
(let [ids (map (comp seat-id parse-seat) input)
      min (reduce min ids)
      max (reduce max ids)
      lookup (set ids)]
  (first (filter (partial (complement contains?) lookup) (range min max)))
  )
