(ns advent-of-code-2020.day-12
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.math.numeric-tower :as math]))

(defn parse-instruction [instr]
  [(keyword (str (first instr)))
   (read-string (apply str (rest instr)))]
  )

(def input
  (->> "day-12.txt"
       (io/resource)
       (slurp)
       (string/split-lines)
       (map parse-instruction)
       )
  )

;part 1
(defn apply-instruction-1 [ctx [action arg]]
  (case action
    :N (update ctx :y #(+ % arg))
    :S (update ctx :y #(- % arg))
    :E (update ctx :x #(+ % arg))
    :W (update ctx :x #(- % arg))
    :L (update ctx :deg #(mod (- % arg) 360))
    :R (update ctx :deg #(mod (+ % arg) 360))
    :F (case (:deg ctx)
         0   (apply-instruction-1 ctx [:N arg])
         90  (apply-instruction-1 ctx [:E arg])
         180 (apply-instruction-1 ctx [:S arg])
         270 (apply-instruction-1 ctx [:W arg])
         )
    )
  )

(defn apply-instructions-1 [ctx instrs]
  (reduce apply-instruction-1 ctx instrs)
  )

(let [result (apply-instructions-1 {:deg 90, :x 0, :y 0} input)]
  (+ (math/abs (:x result)) (math/abs (:y result)))
  )

;part 2
(defn rotate-l [{:keys [dx dy] :as ctx}]
  (-> ctx
      (assoc :dx (- dy))
      (assoc :dy dx))
  )

(defn rotate-r [{:keys [dx dy] :as ctx}]
  (-> ctx
      (assoc :dx dy)
      (assoc :dy (- dx))
      )
  )

(defn apply-instruction-2 [ctx [action arg]]
  (prn ctx [action arg])
  (case action
    :N (update ctx :dy #(+ % arg))
    :S (update ctx :dy #(- % arg))
    :E (update ctx :dx #(+ % arg))
    :W (update ctx :dx #(- % arg))
    :L (reduce (fn [ctx' _] (rotate-l ctx')) ctx (range (/ arg 90)))
    :R (reduce (fn [ctx' _] (rotate-r ctx')) ctx (range (/ arg 90)))
    :F (-> ctx
           (update :x #(+ % (* (:dx ctx) arg)))
           (update :y #(+ % (* (:dy ctx) arg)))
           )
    )
  )

(defn apply-instructions-2 [ctx instrs]
  (reduce apply-instruction-2 ctx instrs)
  )

(let [result (apply-instructions-2 {:x 0, :y 0, :dx 10 :dy 1} input)]
  (+ (math/abs (:x result)) (math/abs (:y result)))
  )
