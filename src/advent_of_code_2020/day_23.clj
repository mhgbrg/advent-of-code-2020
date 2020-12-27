(ns advent-of-code-2020.day-23
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def input
  (-> "day-23.txt"
      (io/resource)
      (slurp)
      (string/split-lines)
      (first)
      (string/split #"")
      (->> (map read-string))
      )
  )

(defn shift [[fst & rest]]
  (concat rest [fst])
  )

(defn find-target [cups current]
  (let [lower (filter #(< % current) cups)]
    (if (empty? lower)
      (apply max cups)
      (apply max lower)
      )
    )
  )

(defn move [cups]
  (let [current (first cups)
        to-move (take 3 (drop 1 cups))
        without-first-4 (drop 4 cups)
        target (find-target without-first-4 current)
        [after-first-4 [_ & rest]] (split-with (comp not #{target}) without-first-4)
        cups' (concat [current] after-first-4 [target] to-move rest)]
    (shift cups')
    )
  )

;part 1
(let [final (reduce (fn [cups _] (move cups)) input (range 100))
      [pre [_ & post]] (split-with (comp not #{1}) final)]
  (string/join (concat post pre))
  )

;part 2
;See day_23.go
