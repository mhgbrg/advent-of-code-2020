(ns advent-of-code-2020.day-15)

(def input [6 13 1 15 2 0])

;part 1
(defn positions [val coll]
  (keep-indexed #(when (= val %2) %1) coll)
  )

(map-indexed
  vector
  (reduce
    (fn [series _round]
      (let [prev (last series)
            [r2 r1] (reverse (positions prev series))
            next (if (not (nil? r1)) (- r2 r1) 0)]
        (conj series next)
        )
      )
    input
    (range (count input) 2020)
    )
  )

;part 2
(loop [prev (last input)
       round (count input)
       seen (into {} (map-indexed #(vector %2 (vector %1)) input))]
  (when (= 0 (mod round 1000000)) (prn round prev))
  (if (= 30000000 round)
    prev
    (let [[r1 r2] (get seen prev)
          n (if (not (nil? r2)) (- r2 r1) 0)]
      (recur
        n
        (inc round)
        (update
          seen
          n
          (fn [rs r]
            (if (< (count rs) 2)
              (vec (conj rs r))
              (vec (next (conj rs r)))
              )
            )
          round
          )
        )
      )
    )
  )
