(ns advent-of-code-2020.day-21
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [[_ ingredients allergens] (re-matches #"([a-z\s]*) \(contains ([a-z\s,]*)\)" line)]
    {:ingredients (string/split ingredients #" ")
     :allergens (string/split allergens #", ")
     }
    )
  )

(def input
  (->> "day-21.txt"
       (io/resource)
       (slurp)
       (string/split-lines)
       (map parse-line)
       )
  )

(def allergens-to-ingredients
  (reduce
    (fn [acc [allergen ingredients]]
      (update
        acc
        allergen
        (fn [prev]
          (if (empty? prev)
            ingredients
            (set/intersection prev ingredients)
            )
          )
        )
      )
    {}
    (mapcat
      (fn [m]
        (for [allergen (:allergens m)]
          [allergen (set (:ingredients m))]
          )
        )
      input
      )
    )
  )

;part 1
(def not-allergens
  (set/difference
    (set (mapcat :ingredients input))
    (reduce set/union (vals allergens-to-ingredients))
    )
  )

(count (filter not-allergens (mapcat :ingredients input)))

;part 2
(def allergens
  (loop [assignments {}]
    (let [ingredient-assigned? (set (vals assignments))
          allergens-to-ingredients' (map
                                      (fn [[allergen ingredients]] [allergen (remove ingredient-assigned? ingredients)])
                                      (apply dissoc allergens-to-ingredients (keys assignments))
                                      )
          [allergen ingredients] (first (sort-by #(count (second %)) allergens-to-ingredients'))]
      (cond
        (nil? allergen) assignments
        (= 1 (count ingredients)) (recur (assoc assignments allergen (first ingredients)))
        :else (throw (AssertionError. "more than one ingredient for allergen"))
        )
      )
    )
  )

(string/join "," (map second (sort-by first (seq allergens))))
