(ns advent-of-code-2020.day-17
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

;part 1
(def input
  (->> "day-17.txt"
       (io/resource)
       (slurp)
       (string/split-lines)
       (mapv #(string/split % #""))
       (map-indexed
         (fn [y row]
           (map-indexed (fn [x cell] [[x y 0] (case cell "#" :active nil)]) row)
           )
         )
       (apply concat)
       (into {})
       )
  )

(defn adjacent-coords [[x y z]]
  (for [dx (range -1 2)
        dy (range -1 2)
        dz (range -1 2)
        :when (not= 0 dx dy dz)
        :let [x' (+ x dx)
              y' (+ y dy)
              z' (+ z dz)]]
    [x' y' z']
    )
  )

(defn count-active-adjacent [state coord]
  (->> coord
       (adjacent-coords)
       (map (partial get state))
       (remove nil?)
       (count))
  )

(defn next-cube-state [state coord]
  (let [num-active-adjacent (count-active-adjacent state coord)]
    (case (get state coord)
      :active (if (#{2 3} num-active-adjacent) :active nil)
      nil (if (= 3 num-active-adjacent) :active nil)
      )
    )
  )

(defn limits [state]
  (let [[x-min y-min z-min] (->> state
                                 (keys)
                                 (apply map list)
                                 (map #(apply min %))
                                 (map dec))
        [x-max y-max z-max] (->> state
                                 (keys)
                                 (apply map list)
                                 (map #(apply max %))
                                 (map #(+ 2 %)))]
    {:x [x-min x-max]
     :y [y-min y-max]
     :z [z-min z-max]}
    )
  )

(defn next-state [state]
  (->> (let [lims (limits state)]
         (for [x (apply range (:x lims))
               y (apply range (:y lims))
               z (apply range (:z lims))
               :let [coord [x y z]]]
           [coord (next-cube-state state coord)]
           )
         )
       (into {})
       )
  )

(defn show-state [state z]
  (let [lims (limits state)]
    (string/join
      "\n"
      (for [y (apply range (:y lims))]
        (string/join
          ""
          (for [x (apply range (:x lims))]
            (case (get state [x y z])
              :active "#"
              nil "."
              )
            )
          )
        )
      )
    )
  )

(->> (reduce
       (fn [state _cycle] (next-state state))
       input
       (range 6)
       )
     (vals)
     (remove nil?)
     (count)
     )

;part 2
(def input-2
  (->> "day-17.txt"
       (io/resource)
       (slurp)
       (string/split-lines)
       (mapv #(string/split % #""))
       (map-indexed
         (fn [y row]
           (map-indexed (fn [x cell] [[x y 0 0] (case cell "#" :active nil)]) row)
           )
         )
       (apply concat)
       (into {})
       )
  )

(defn adjacent-coords-2 [[x y z w]]
  (for [dx (range -1 2)
        dy (range -1 2)
        dz (range -1 2)
        dw (range -1 2)
        :when (not= 0 dx dy dz dw)
        :let [x' (+ x dx)
              y' (+ y dy)
              z' (+ z dz)
              w' (+ w dw)]]
    [x' y' z' w']
    )
  )

(defn limits-2 [state]
  (let [[x-min y-min z-min w-min] (->> state
                                 (keys)
                                 (apply map list)
                                 (map #(apply min %))
                                 (map dec))
        [x-max y-max z-max w-max] (->> state
                                 (keys)
                                 (apply map list)
                                 (map #(apply max %))
                                 (map #(+ 2 %)))]
    {:x [x-min x-max]
     :y [y-min y-max]
     :z [z-min z-max]
     :w [w-min w-max]}
    )
  )

(defn count-active-adjacent-2 [state coord]
  (->> coord
       (adjacent-coords-2)
       (map (partial get state))
       (remove nil?)
       (count))
  )

(defn next-cube-state-2 [state coord]
  (let [num-active-adjacent (count-active-adjacent-2 state coord)]
    (case (get state coord)
      :active (if (#{2 3} num-active-adjacent) :active nil)
      nil (if (= 3 num-active-adjacent) :active nil)
      )
    )
  )

(defn next-state-2 [state]
  (->> (let [lims (limits-2 state)]
         (for [x (apply range (:x lims))
               y (apply range (:y lims))
               z (apply range (:z lims))
               w (apply range (:w lims))
               :let [coord [x y z w]]]
           [coord (next-cube-state-2 state coord)]
           )
         )
       (into {})
       )
  )

(->> (reduce
       (fn [state _cycle] (next-state-2 state))
       input-2
       (range 6)
       )
     (vals)
     (remove nil?)
     (count)
     )