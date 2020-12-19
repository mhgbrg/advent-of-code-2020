(ns advent-of-code-2020.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.walk :as walk]))

(def adapters
  (let [input (->> "day-10.txt"
                   (io/resource)
                   (slurp)
                   (string/split-lines)
                   (map read-string)
                   )]
    (conj input 0 (+ 3 (reduce max input)))
    )
  )

(defn diffs [adapters]
  (map #(- (second %) (first %)) (partition 2 1 (sort adapters)))
  )

;part 1
(let [freqs (frequencies (diffs adapters))]
  (* (freqs 1) (freqs 3))
  )

;part 2
(->> adapters
     (diffs)
     (string/join "")
     (re-seq #"1+3")
     (map {"13" 1, "113" 2, "1113" 4, "11113" 7})
     (reduce *)
     )
