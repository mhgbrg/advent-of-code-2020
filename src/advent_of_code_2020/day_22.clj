(ns advent-of-code-2020.day-22
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-deck [str]
  (mapv read-string (string/split-lines (string/replace str #"Player \d:\n" "")))
  )

(def input
  (let [[p1 p2] (-> "day-22.txt"
                    (io/resource)
                    (slurp)
                    (string/split #"\n\n")
                    (->> (map parse-deck)))]
    {:player-1 p1
     :player-2 p2}
    )
  )

(defn score [deck]
  (apply + (map #(* %1 (inc %2)) (reverse deck) (range)))
  )

;part 1
(loop [d1 (:player-1 input)
       d2 (:player-2 input)]
  (cond
    ; Player 1 wins game
    (empty? d2) (score d1)
    ; Player 2 wins game
    (empty? d1) (score d2)
    ; Game continues
    :else (let [[c1 & r1] d1
                [c2 & r2] d2]
            (cond
              ; Player 1 wins round
              (> c1 c2) (recur (concat r1 [c1 c2]) r2)
              ; Player 2 wins round
              (< c1 c2) (recur r1 (concat r2 [c2 c1]))
              )
            )
    )
  )

;part 2
(defn game [d1 d2 seen?]
  (cond
    ; No cards left for player 2, player 1 wins game
    (empty? d2) [:player-1 (score d1)]
    ; No cards left for player 1, player 2 wins game
    (empty? d1) [:player-2 (score d2)]
    ; Seen configuration before, player 1 wins game
    (seen? [d1 d2]) [:player-1 (score d1)]
    ; New configuration, play round
    :else (let [[c1 & r1] d1
                [c2 & r2] d2]
            (if (and (>= (count r1) c1) (>= (count r2) c2))
              ; Enough cards left, play a sub-game to determine winner of round
              (let [[winner _score] (game (take c1 r1) (take c2 r2) #{})]
                (case winner
                  ; Player 1 wins round by winning sub-game
                  :player-1 (recur (concat r1 [c1 c2]) r2 (conj seen? [d1 d2]))
                  ; Player 2 wins round by winning sub-game
                  :player-2 (recur r1 (concat r2 [c2 c1]) (conj seen? [d1 d2]))
                  )
                )
              ; Not enough cards left, play round normally
              (cond
                ; Player 1 wins round
                (> c1 c2) (recur (concat r1 [c1 c2]) r2 (conj seen? [d1 d2]))
                ; Player 2 wins round
                (< c1 c2) (recur r1 (concat r2 [c2 c1]) (conj seen? [d1 d2]))
                )
              )
            )
    )
  )

(game (:player-1 input) (:player-2 input) #{})
