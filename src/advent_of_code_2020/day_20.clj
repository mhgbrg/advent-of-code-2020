(ns advent-of-code-2020.day-20
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as math]))

;Parse input
(defn parse-tile [tile]
  (let [[fst & rest] (string/split-lines tile)]
    [(read-string (first (re-seq #"\d+" fst)))
     (mapv vec rest)]
    )
  )

(def input
  (->> "day-20.txt"
       (io/resource)
       (slurp)
       (#(string/split % #"\n\n"))
       (map parse-tile)
       (into {})
       )
  )

;Image transformations
(defn rotate [img]
  (let [n (count img)]
    (vec (for [i (range n)]
      (vec (for [j (range n)]
        (get-in img [(dec (- n j)) i])
        )
      ))
    ))
  )

(defn flip [img]
  (apply mapv vector img)
  )

;Graph creation
(defn all-versions [[id img]]
  [[(str id ".0") img]
   [(str id ".90") (rotate img)]
   [(str id ".180") (rotate (rotate img))]
   [(str id ".270") (rotate (rotate (rotate img)))]
   [(str id ".0f") (flip img)]
   [(str id ".90f") (flip (rotate img))]
   [(str id ".180f") (flip (rotate (rotate img)))]
   [(str id ".270f") (flip (rotate (rotate (rotate img))))]]
  )

(defn extract-id [id]
  (if (int? id)
    id
    (read-string (first (string/split id #"\.")))
    )
  )

(defn same-tile? [id-1 id-2]
  (=
    (extract-id id-1)
    (extract-id id-2)
    )
  )

(defn connects? [img1 img2 dir]
  (case dir
    :up (= (first img1) (last img2))
    :down (= (last img1) (first img2))
    :left (= (first (flip img1)) (last (flip img2)))
    :right (= (last (flip img1)) (first (flip img2)))
    )
  )

(def graph
  (let [original-tiles input
        all-tiles (into {} (mapcat all-versions input))]
    (into
      {}
      (for [[id-1 img-1] all-tiles]
        [id-1
         {:up    (first (for [[id-2 img-2] all-tiles
                              :when (and (not (same-tile? id-1 id-2)) (connects? img-1 img-2 :up))]
                          id-2
                          ))
          :down  (first (for [[id-2 img-2] all-tiles
                              :when (and (not (same-tile? id-1 id-2)) (connects? img-1 img-2 :down))]
                          id-2
                          ))
          :left  (first (for [[id-2 img-2] all-tiles
                              :when (and (not (same-tile? id-1 id-2)) (connects? img-1 img-2 :left))]
                          id-2
                          ))
          :right (first (for [[id-2 img-2] all-tiles
                              :when (and (not (same-tile? id-1 id-2)) (connects? img-1 img-2 :right))]
                          id-2
                          ))
          }]
        )
      )
    )
  )

graph

;part 1
(def corners
  (filter
    (fn [[_ {:keys [up down left right]}]] (= 2 (count (remove nil? [up down left right]))))
    graph
    )
  )

corners

;(apply * (map first corners))

(/ 9 3)

;part 2
(defn assemble [top-left-id]
  (let [dim (math/sqrt (/ (count graph) 8))
        stitched (mapv (fn [_] (vec (repeat dim 0))) (repeat dim 0))]
    (reduce
      (fn [pic [x y]] (do
                        ;(prn pic [x y])
                        (assoc-in
                          pic [y x]
                          (cond
                            (= 0 x y) (get-in pic [y x])
                            (= 0 x) (let [above (get-in pic [(dec y) x])]
                                      (:down (get graph above))
                                      )
                            :else (let [left (get-in pic [y (dec x)])]
                                    (:right (get graph left))
                                    )
                            )
                          )
                        )
        )
        (assoc-in stitched [0 0] top-left-id)
        (combo/cartesian-product (range dim) (range dim))
        )
      )
    )

(defn img-to-str [img]
  (string/join "\n" (map #(string/join "" %) img))
  )

(defn merge-horizontally [img-1 img-2]
  (mapv (comp vec concat) img-1 img-2)
  )

(defn merge-vertically [img-1 img-2]
  (vec (concat img-1 img-2))
  )

(defn remove-borders [img]
  (->> img
       (next)
       (butlast)
       (mapv (comp vec next))
       (mapv (comp vec butlast))
       )
  )

(defn safe-subs [s start]
  (if (nil? s)
    ""
    (subs s (min start (dec (count s))))
    )
  )

(defn is-monster? [img x y]
  (and
    (re-matches #"..................#.*" (safe-subs (get img y) x))
    (re-matches #"#....##....##....###.*" (safe-subs (get img (+ 1 y)) x))
    (re-matches #".#..#..#..#..#..#.*" (safe-subs (get img (+ 2 y)) x))
    )
  )

;example: 2971.0
;real: 2633.0
(let [top-left-id "2633.0"
      grid-of-ids (assemble top-left-id)
      all-tiles (into {} (mapcat all-versions input))
      grid-of-imgs (mapv (partial mapv all-tiles) grid-of-ids)
      without-borders (mapv (partial mapv remove-borders) grid-of-imgs)
      grid-of-chars (reduce merge-vertically (mapv (partial reduce merge-horizontally) without-borders))
      img (mapv string/join grid-of-chars)
      transformed (mapv
                    #(mapv string/join %)
                    [img
                     (rotate img)
                     (rotate (rotate img))
                     (rotate (rotate (rotate img)))
                     (flip img)
                     (flip (rotate img))
                     (flip (rotate (rotate img)))
                     (flip (rotate (rotate (rotate img))))]
                    )
      ]
  (for [img' transformed]
    (-
      (count (filter #{\#} (apply concat img)))
      (apply + (for [x (range (count img))
                     y (range (count img))
                     :when (is-monster? img' x y)]
                 15
                 )
             )
      )
    )
  )
