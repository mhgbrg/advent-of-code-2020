(ns advent-of-code-2020.day-14
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.core.match :refer [match]]))

(defn parse-mask [mask]
  (->> (reverse mask)
       (map-indexed vector)
       )
  )

(defn parse-instruction [instr]
  (let [[_ op _ i arg] (re-matches #"(mask|mem)(\[(\d+)\])? = (.*)" instr)]
    (case op
      "mask" [:mask nil (parse-mask arg)]
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

(defn apply-instr [{:keys [mask] :as ctx} [op i arg]]
  (case op
    :mask (assoc ctx :mask arg)
    :mem (assoc ctx i (apply-mask mask arg))
    )
  )

(let [ctx (reduce apply-instr {:mask '()} input)]
  (apply + (vals (dissoc ctx :mask)))
  )

;part 2
(defn apply-mask-2 [mask x]
  (reduce
    (fn [xs [i m]]
      (case m
        nil xs
        \0 xs
        \1 (map #(bit-set % i) xs)
        \X (concat
             (map #(bit-clear % i) xs)
             (map #(bit-set % i) xs)
             )
        )
      )
    (list x)
    mask
    )
  )

(defn apply-instr-2 [{:keys [mask] :as ctx} [op i arg]]
  (case op
    :mask (assoc ctx :mask arg)
    :mem (reduce
           (fn [ctx' i'] (assoc ctx' i' arg))
           ctx
           (apply-mask-2 mask i)
           )
    )
  )

(let [ctx (reduce apply-instr-2 {:mask '()} input)]
  (apply + (vals (dissoc ctx :mask)))
  )
