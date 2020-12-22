(ns advent-of-code-2020.day-16
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-ticket [ticket]
  (map read-string (string/split ticket #","))
  )

(defn parse-rule [rule]
  (let [[_ field to1 from1 to2 from2] (re-matches #"([a-z\s]+): (\d+)-(\d+) or (\d+)-(\d+)" rule)]
    {:field  field
     :range1 [(read-string to1) (read-string from1)]
     :range2 [(read-string to2) (read-string from2)]}
    )
  )

(def input
  (let [[rules [_ your] [_ & nearby]]
        (->> "day-16.txt"
             (io/resource)
             (slurp)
             (#(string/split % #"\n\n"))
             (map string/split-lines))]
    {:rules  (map parse-rule rules)
     :your   (parse-ticket your)
     :nearby (map parse-ticket nearby)}
    )
  )

(defn value-matches-rule? [rule n]
  (or
    (<= (first (:range1 rule)) n (second (:range1 rule)))
    (<= (first (:range2 rule)) n (second (:range2 rule)))
    )
  )

(defn value-matches-some-rule? [rules n]
  (some true? (map #(value-matches-rule? % n) rules))
  )

;part 1
(->> (apply concat (:nearby input))
     (filter (comp not (partial value-matches-some-rule? (:rules input))))
     (apply +)
     )

;part 2
(defn valid-ticket? [rules ticket]
  (->> ticket
       (map (partial value-matches-some-rule? rules))
       (every? true?)
       )
  )

(defn rule-matches-all-values? [values rule]
  (->> values
       (map (partial value-matches-rule? rule))
       (every? true?)
       )
  )

(defn find-matching-rules [rules values]
  (filter
    (partial rule-matches-all-values? values)
    rules
    )
  )

(defn assign-rules [matching-rules-per-pos]
  (loop [matching-rules-per-pos matching-rules-per-pos
         assigned {}]
    (if (= 0 (count matching-rules-per-pos))
      assigned
      (let [[i rules] (first (filter #(= 1 (count (second %))) matching-rules-per-pos))
            rule (first rules)]
        (if (nil? rule)
          (throw (AssertionError. "no pos with only one rule left"))
          (recur
            (-> (into {} (map (fn [[k v]] [k (remove #{rule} v)]) matching-rules-per-pos))
                (dissoc i))
            (assoc assigned i rule)
            )
          )
        )
      )
    )
  )

(let [valid (filter (partial valid-ticket? (:rules input)) (:nearby input))
      by-fields (apply map list valid)
      matching-rules-per-pos (into {} (map-indexed #(vector %1 (find-matching-rules (:rules input) %2)) by-fields))]
  (->> (assign-rules matching-rules-per-pos) ; returns a map on the form {i rule}
       (filter #(string/starts-with? (:field (second %)) "departure")) ; only keep departure fields
       (map first) ; drop field names, keep indices
       (map #(nth (:your input) %)) ; get corresponding values from ticket
       (apply *)
       )
  )
