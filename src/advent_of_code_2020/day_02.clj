(ns advent-of-code-2020.day-02
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-line [line]
  (let [[_ i1 i2 letter password] (re-find #"(\d+)-(\d+) (.): (.*)" line)]
    {:i1 (read-string i1)
     :i2 (read-string i2)
     :letter (first letter)
     :password password}
  )
)

(def input (map parse-line (string/split-lines (slurp (io/resource "day-02.txt")))))

;; part 1
(defn password-valid-part-1?
  [{min :i1
    max :i2
    letter :letter
    password :password}]
  (let [letter-count (get (frequencies password) letter 0)]
    (<= min letter-count max)
  )
)

(count (filter true? (map password-valid-part-1? input)))

;; part 2
(defn password-valid-part-2?
  [{pos1 :i1
    pos2 :i2
    letter :letter
    password :password}]
  (let [first-match (= letter (nth password (dec pos1)))
        second-match (= letter (nth password (dec pos2)))]
    (not= first-match second-match)
  )
)

(count (filter true? (map password-valid-part-2? input)))
