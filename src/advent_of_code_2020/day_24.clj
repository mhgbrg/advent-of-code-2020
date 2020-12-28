(ns advent-of-code-2020.day-24
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.algo.generic.functor :as functor]
            [clojure.set :as set]))

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

(defn instruction-to-coord [instr]
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

(def initial-state
  (->> input
       (map instruction-to-coord)
       (frequencies)
       (functor/fmap #(case (mod % 2) 0 :white 1 :black))
       (filter (fn [[_ color]] (= color :black)))
       (map first)
       (into #{})
       )
  )

;part 1
(count initial-state)

;part 2
(defn get-tile [state coord]
  (if (contains? state coord)
    :black
    :white
    )
  )

(defn set-tile [state coord color]
  (if (= color :black)
    (conj state coord)
    (disj state coord)
    )
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
  (reduce
    (fn [state' coord]
      (let [tile (next-tile state coord)]
        (set-tile state' coord tile)
        )
      )
    state
    (set/union state (set (mapcat adjacent-coords state)))
    )
  )

(count
  (reduce
    (fn [state _day] (next-state state))
    initial-state
    (range 100))
  )