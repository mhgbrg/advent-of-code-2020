(ns advent-of-code-2020.day-17
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def input
  (->> "day-17.txt"
       (io/resource)
       (slurp)
       (string/split-lines)
       (mapv #(string/split % #""))
       )
  )

;part 1
(def input-3d
  (->> input
       (map-indexed
         (fn [y row]
           (map-indexed (fn [x cell] [[x y 0] (case cell "#" :active nil)]) row)
           )
         )
       (apply concat)
       (remove #(nil? (second %)))
       (into {})
       )
  )

(defn adjacent-coord-3d [[x y z]]
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

(defn count-active-adjacent-3d [state coord]
  (->> coord
       (adjacent-coord-3d)
       (map (partial get state))
       (remove nil?)
       (count))
  )

(defn next-cube-state-3d [state coord]
  (let [num-active-adjacent (count-active-adjacent-3d state coord)]
    (case (get state coord)
      :active (if (#{2 3} num-active-adjacent) :active nil)
      nil (if (= 3 num-active-adjacent) :active nil)
      )
    )
  )

(defn limits-3d [state]
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

(defn next-state-3d [state]
  (->> (let [lims (limits-3d state)]
         (for [x (apply range (:x lims))
               y (apply range (:y lims))
               z (apply range (:z lims))
               :let [coord [x y z]]]
           [coord (next-cube-state-3d state coord)]
           )
         )
       (remove #(nil? (second %)))
       (into {})
       )
  )

(->> (reduce
       (fn [state _cycle] (next-state-3d state))
       input-3d
       (range 6)
       )
     (vals)
     (remove nil?)
     (count)
     )

;part 2
(def input-4d
  (->> input
       (map-indexed
         (fn [y row]
           (map-indexed (fn [x cell] [[x y 0 0] (case cell "#" :active nil)]) row)
           )
         )
       (apply concat)
       (remove #(nil? (second %)))
       (into {})
       )
  )

(defn adjacent-coords-4d [[x y z w]]
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

(defn count-active-adjacent-4d [state coord]
  (->> coord
       (adjacent-coords-4d)
       (map (partial get state))
       (remove nil?)
       (count))
  )

(defn next-cube-state-4d [state coord]
  (let [num-active-adjacent (count-active-adjacent-4d state coord)]
    (case (get state coord)
      :active (if (#{2 3} num-active-adjacent) :active nil)
      nil (if (= 3 num-active-adjacent) :active nil)
      )
    )
  )

(defn limits-4d [state]
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

(defn next-state-4d [state]
  (->> (let [lims (limits-4d state)]
         (for [x (apply range (:x lims))
               y (apply range (:y lims))
               z (apply range (:z lims))
               w (apply range (:w lims))
               :let [coord [x y z w]]]
           [coord (next-cube-state-4d state coord)]
           )
         )
       (remove #(nil? (second %)))
       (into {})
       )
  )

(->> (reduce
       (fn [state _cycle] (next-state-4d state))
       input-4d
       (range 6)
       )
     (vals)
     (remove nil?)
     (count)
     )
