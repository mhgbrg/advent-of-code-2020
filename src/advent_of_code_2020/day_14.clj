(ns advent-of-code-2020.day-14
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.core.match :refer [match]]))

(defn parse-instruction [instr]
  (let [[_ op _ i arg] (re-matches #"(mask|mem)(\[(\d+)\])? = (.*)" instr)]
    (case op
      "mask" [:mask nil arg]
      "mem" [:mem (read-string i) (read-string arg)]
      )
    )
  )

(def input
  (->> "day-14.txt"
       (io/resource)
       (slurp)
       (string/split-lines)
       (map parse-instruction)
       )
  )

;part 1
(defn parse-mask [mask]
  (->> (reverse mask)
       (map-indexed (fn [i m] [i m]))
       (filter #(not (nil? (second %))))
       )
  )

(defn apply-mask [mask x]
  (reduce
    (fn [x' [i m]]
      (case m
        \0 (bit-clear x' i)
        \1 (bit-set x' i)
        \X x'
        )
      )
    x
    mask
    )
  )

(defn apply-instr [{:keys [mask mem] :as ctx} [op i arg]]
  (case op
    :mask (assoc ctx :mask (parse-mask arg))
    :mem  (assoc ctx :mem (assoc mem i (apply-mask mask arg)))
    )
  )

(let [{:keys [mem]} (reduce apply-instr {:mask '() :mem {}} input)]
  (apply + (vals mem))
  )

;part 2
(defn apply-mask-2 [mask x]
  (loop [mask' mask
         xs (list x)]
    (let [[[i m] & rest] mask']
      (case m
        nil xs
        \0 (recur rest xs)
        \1 (recur rest (map #(bit-set % i) xs))
        \X (recur
             rest
             (concat
               (map #(bit-clear % i) xs)
               (map #(bit-set % i) xs)
               )
             )
        )
      )
    )
  )

(defn apply-instr-2 [{:keys [mask mem] :as ctx} [op i arg]]
  (case op
    :mask (assoc ctx :mask (parse-mask arg))
    :mem (assoc
           ctx
           :mem
           (reduce
             (fn [mem' i'] (assoc mem' i' arg))
             mem
             (apply-mask-2 mask i)
             )
           )
    )
  )

(let [{:keys [mem]} (reduce apply-instr-2 {:mask '() :mem {}} input)]
  (apply + (vals mem))
  )
