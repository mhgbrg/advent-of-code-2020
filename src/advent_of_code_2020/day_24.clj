(ns advent-of-code-2020.day-24
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.algo.generic.functor :as functor]
            [clojure.math.combinatorics :as combo]))

(defn parse-instruction [instr]
  (map keyword (re-seq #"e|se|sw|w|nw|ne" instr))
  )

(def input
  (->> "day-24.txt"
       (io/resource)
       (slurp)
       (string/split-lines)
       (map parse-instruction)
       )
  )

(defn coords [instr]
  (reduce
    (fn [[x y] c]
      (case c
        :e  [(+ x 2) y]
        :se [(+ x 1) (+ y 1)]
        :sw [(- x 1) (+ y 1)]
        :w  [(- x 2) y]
        :nw [(- x 1) (- y 1)]
        :ne [(+ x 1) (- y 1)]
        )
      )
    [0 0]
    instr
    )
  )

;part 1
(->> input
     (map coords)
     (frequencies)
     (vals)
     (filter #(= 1 (mod % 2)))
     (count)
     )

;part 2
(defn get-tile [state [x y]]
  (or (get state [x y]) :white)
  )

(defn adjacent-coords [[x y]]
  (let [ds [[2 0]
            [1 1]
            [-1 1]
            [-2 0]
            [-1 -1]
            [1 -1]]]
    (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) ds)
    )
  )

(defn adjacent-tiles [state coord]
  (map
    (partial get-tile state)
    (adjacent-coords coord)
    )
  )

(defn next-tile [state coord]
  (let [num-blacks (count (filter #{:black} (adjacent-tiles state coord)))]
    (case (get-tile state coord)
      :black (if (or (= num-blacks 0) (> num-blacks 2)) :white :black)
      :white (if (= num-blacks 2) :black :white)
      )
    )
  )

(defn next-state [state]
  (let [[xs ys] (apply map list (keys state))
        x-min (apply min xs)
        x-max (apply max xs)
        y-min (apply min ys)
        y-max (apply max ys)]
    (reduce
      (fn [state' coord]
        (assoc
          state'
          coord
          (next-tile state coord)
          )
        )
      state
      (combo/cartesian-product (range (- x-min 2) (+ x-max 3)) (range (- y-min 1) (+ y-max 2)))
      )
    )
  )

(let [initial-state (->> input
                         (map coords)
                         (frequencies)
                         (functor/fmap #(case (mod % 2) 0 :white 1 :black)))
      final-state (reduce
                    (fn [state day]
                      (println "day" (inc day))
                      (next-state state))
                    initial-state
                    (range 100))]
  (->> final-state
       (vals)
       (filter #{:black})
       (count)
       )
  )