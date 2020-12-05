(ns advent-of-code-2020.day-01
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(defn read-input [filename] (map read-string (string/split-lines (slurp (io/resource filename)))))

(def input (read-input "day-01.txt"))

;; part 1
(first
  (for [x input
        y input
        :when (= 2020 (+ x y))]
    (* x y)
  )
)

;; part 2
(first
  (for [x input
        y input
        z input
        :when (= 2020 (+ x y z))]
    (* x y z)
  )
)
