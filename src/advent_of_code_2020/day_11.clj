(ns advent-of-code-2020.day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-map [str]
  (->> str
       (string/split-lines)
       (map #(string/split % #""))
       (map (comp vec (partial map keyword)))
       (vec))
  )

(def input
  (->> "day-11.txt"
       (io/resource)
       (slurp)
       (parse-map)
       )
  )

(defn progress [next-state m]
  (let [width (count (first m))
        height (count m)]
    (vec
      (for [y (range 0 height)]
        (vec
          (for [x (range 0 width)]
            (next-state m y x)
            )
          )
        )
      )
    )
  )

(defn progress-until-steady-state [next-state m-init]
  (loop [m m-init]
    (let [m' (progress next-state m)]
      (if (= m m')
        m
        (recur m')
        )
      )
    )
  )

(defn count-occupied [occupied? m y x]
  (let [deltas '([-1 -1]
                 [-1 0]
                 [-1 1]
                 [0 -1]
                 [0 1]
                 [1 -1]
                 [1 0]
                 [1 1])]
    (->> deltas
         (map (partial occupied? m [y x]))
         (filter true?)
         (count)
         )
    )
  )

;part 1
(defn occupied-1? [m [y x] [dy dx]]
  (case (get-in m [(+ y dy) (+ x dx)])
    :. false
    :L false
    :# true
    nil false
    )
  )

(defn next-state-1 [m y x]
  (let [occupied (count-occupied occupied-1? m y x)]
    (case (get-in m [y x])
      :. :.
      :L (if (= occupied 0) :# :L)
      :# (if (>= occupied 4) :L :#)
      )
    )
  )

(->> input
     (progress-until-steady-state next-state-1)
     (reduce concat)
     (filter #(= :# %))
     (count)
     )

;part 2
(defn occupied-2? [m [init-y init-x] [dy dx]]
  (loop [y (+ init-y dy), x (+ init-x dx)]
    (case (get-in m [y x])
      :.  (recur (+ y dy) (+ x dx))
      :L  false
      :#  true
      nil false
      )
    )
  )

(defn next-state-2 [m y x]
  (let [occupied (count-occupied occupied-2? m y x)]
    (case (get-in m [y x])
      :. :.
      :L (if (= occupied 0) :# :L)
      :# (if (>= occupied 5) :L :#)
      )
    )
  )

(->> input
     (progress-until-steady-state next-state-2)
     (reduce concat)
     (filter #(= :# %))
     (count)
     )
