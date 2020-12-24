(ns advent-of-code-2020.day-19
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))


(defn parse-rule [rule]
  (let [[id logic] (string/split rule #": ")
        clauses (string/split logic #" \| ")]
    [(read-string id)
     (map #(map read-string (string/split % #" ")) clauses)]
    )
  )

(def input
  (let [[rules messages] (->> "day-19.txt"
                              (io/resource)
                              (slurp)
                              (#(string/split % #"\n\n"))
                              (map string/split-lines))]
    {:rules    (into {} (map parse-rule rules))
     :messages messages}
    )
  )

(defn expand-rule [rules x]
  (if (string? x)
    (list x)
    (apply
      concat
      (for [clause (get rules x)]
        (map
          (partial apply str)
          (apply
            combo/cartesian-product
            (for [y clause]
              (expand-rule rules y)
              )
            )
          )
        )
      )
    )
  )

;part 1
(let [valid? (set (expand-rule (:rules input) 0))]
  (count (filter valid? (:messages input))))

;part 2
(def matches-31? (set (expand-rule (:rules input) 31)))

(def matches-42? (set (expand-rule (:rules input) 42)))

(defn safe-subs [s start end]
  (subs s start (min end (count s)))
  )

(defn valid? [msg]
  (let [n-31 (count (first matches-31?))
        n-42 (count (first matches-42?))]
    (loop [msg (apply str (reverse msg))
           ctx {:stage :0, :n 0}]
      (case (:stage ctx)
        ; Stage 0: Match a single 31
        :0 (let [substr (apply str (reverse (safe-subs msg 0 n-31)))]
             (if (matches-31? substr)
               (recur
                 (subs msg n-31)
                 (-> ctx (update :n inc) (assoc :stage :1)))
               false
               )
             )
        ; Stage 1: Match n 31s
        :1 (let [substr (apply str (reverse (safe-subs msg 0 n-31)))]
             (if (matches-31? substr)
               (recur
                 (subs msg n-31)
                 (update ctx :n inc))
               (recur
                 msg
                 (assoc ctx :stage :2))
               )
             )
        ; Stage 2: Match n 42s
        :2 (if (= 0 (:n ctx))
             (recur msg (assoc ctx :stage :3))
             (let [substr (apply str (reverse (safe-subs msg 0 n-42)))]
               (if (matches-42? substr)
                 (recur
                   (subs msg n-42)
                   (update ctx :n dec))
                 false
                 )
               )
             )
        ; Stage 3: Match a single 42
        :3 (let [substr (apply str (reverse (safe-subs msg 0 n-42)))]
             (if (matches-42? substr)
               (recur
                 (subs msg n-42)
                 (assoc ctx :stage :4))
               false
               )
             )
        ; Stage 4: Match 42s until the string is empty
        :4 (if (= 0 (count msg))
             true
             (let [substr (apply str (reverse (safe-subs msg 0 n-42)))]
               (if (matches-42? substr)
                 (recur
                   (subs msg n-42)
                   ctx)
                 false
                 )
               )
             )
        )
      )
    )
  )

(count (filter valid? (:messages input)))
