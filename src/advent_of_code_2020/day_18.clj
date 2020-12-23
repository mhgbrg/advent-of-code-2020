(ns advent-of-code-2020.day-18
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def input
  (->> "day-18.txt"
       (io/resource)
       (slurp)
       (string/split-lines)
       )
  )

;part 1
(defn eval-simple-expr-1 [expr]
  (let [parts (string/split expr #"\s")]
    (reduce
      (fn [acc [op n]]
        (case op
          "+" (+ acc (read-string n))
          "*" (* acc (read-string n))
          )
        )
      (read-string (first parts))
      (partition 2 (rest (string/split expr #"\s")))
      )
    )
  )

(defn eval-expr-1 [expr]
  (if (= 0 (count (filter #{\( \)} expr)))
    (eval-simple-expr-1 expr)
    (recur
      (reduce
        (fn [expr' simple-expr]
          (let [result (eval-simple-expr-1 (string/replace simple-expr #"[\(\)]" ""))]
            (string/replace expr' simple-expr (str result))
            )
          )
        expr
        (re-seq #"\([^\(]+?\)" expr)
        )
      )
    )
  )

;test
(for [expr input]
  [expr (eval-expr-1 expr)]
  )

;solution
(apply + (map eval-expr-1 input))

;part 2
(defn eval-simple-expr-2 [expr]
  (apply * (map eval-simple-expr-1 (string/split expr #" \* ")))
  )

(defn eval-expr-2 [expr]
  (if (= 0 (count (filter #{\( \)} expr)))
    (eval-simple-expr-2 expr)
    (recur
      (reduce
        (fn [expr' simple-expr]
          (let [result (eval-simple-expr-2 (string/replace simple-expr #"[\(\)]" ""))]
            (string/replace expr' simple-expr (str result))
            )
          )
        expr
        (re-seq #"\([^\(]+?\)" expr)
        )
      )
    )
  )

;test
(for [expr input]
  [expr (eval-expr-2 expr)]
  )

;solution
(apply + (map eval-expr-2 input))
