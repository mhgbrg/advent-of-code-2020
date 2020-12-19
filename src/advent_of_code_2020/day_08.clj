(ns advent-of-code-2020.day-08
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-line [line]
  (let [[op arg] (string/split line #" ")]
    [(keyword op) (read-string arg)]
    )
  )

(def input
  (->> "day-08.txt"
       (io/resource)
       (slurp)
       (string/split-lines)
       (map parse-line)
       (vec)
       )
  )

(defn execute [ops]
  (loop [{:keys [pc acc seen-pcs] :as ctx} {:pc 0 :acc 0 :seen-pcs #{}}]
    (if (contains? seen-pcs pc)
      [:loop acc]
      (let [[op arg] (get ops pc)
            ctx' (update ctx :seen-pcs conj pc)]
        (case op
          :acc (recur (-> ctx' (update :pc inc) (update :acc + arg)))
          :jmp (recur (-> ctx' (update :pc + arg)))
          :nop (recur (-> ctx' (update :pc inc)))
          [:term acc]
          )
        )
      )
    )
  )

;part 1
(execute input)

;part 2
(defn generate-programs [program]
  (for [i (range 0 (count program))
        :when (contains? #{:jmp :nop} (get-in program [i 0]))]
    (update-in program [i 0] {:jmp :nop, :nop :jmp})
    )
  )

(->> input
     (generate-programs)
     (map execute)
     (filter #(= (first %) :term))
     (first)
     )
