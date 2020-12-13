(ns advent-of-code-2020.day-07
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-line [line]
  (let [[_ source] (re-find #"(.*?) bags contain" line)
        targets (re-seq #"(\d+) (.*?) bags?" line)]
    [source
     (for
       [[_ quantity color] targets]
       [(read-string quantity) color]
       )
     ]
    )
  )

(def input
  (->> "day-07.txt"
       (io/resource)
       (slurp)
       (string/split-lines)
       (map parse-line)
       (into {})
       )
  )

; part 1
(defn has-path-to? [graph target node]
  (if (= node target)
    true
    (some
      true?
      (for [[_ neighbour] (graph node)]
        (has-path-to? graph target neighbour)
        )
      )
    )
  )

(->> (for [node (keys input)] (has-path-to? input "shiny gold" node))
     (filter true?)
     (count)
     (dec)
     )

; part 2
(defn count-nodes [graph node]
  (inc
    (reduce
      +
      (for [[n neighbour] (graph node)]
        (* n (count-nodes graph neighbour))
        )
      )
    )
  )

(dec (count-nodes input "shiny gold"))
