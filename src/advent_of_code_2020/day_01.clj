(ns advent-of-code-2020.day-01
  (:require [clojure.string :refer [split-lines]]
            [clojure.java.io :refer [resource]]))

(defn read-input [filename] (map read-string (split-lines (slurp (resource filename)))))

(def input (read-input "day-01.txt"))

;; part 1
(let [solutions (flatten
                  (for [x input]
                    (for [y input]
                      (if (= 2020 (+ x y)) (* x y))
                    )
                  )
                )
      ]
  (first (remove nil? solutions))
)

;; part 2
(let [solutions (flatten
                  (for [x input]
                    (for [y input]
                      (for [z input]
                        (if (= 2020 (+ x y z)) (* x y z))
                      )
                    )
                  )
                )
      ]
  (first (remove nil? solutions))
)
