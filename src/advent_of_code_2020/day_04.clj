(ns advent-of-code-2020.day-04
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-passport
  [passport]
  (into
    {}
    (map
      (fn [[_ key val]] [(keyword key) val])
      (re-seq #"([a-z]{3}):(\S+)" passport)
      )
    )
  )

(def input
  (map
    parse-passport
    (string/split (slurp (io/resource "day-04.txt")) #"\n\n")
    )
  )

; part 1
(def required-fields [:byr :iyr :eyr :hgt :hcl :ecl :pid])

(defn valid-1?
  [passport]
  (every?
    true?
    (map
      (partial contains? passport)
      required-fields
      )
    )
  )

(->> input
     (map valid-1?)
     (filter true?)
     (count))

; part 2
(defn integer-between?
  [val min max]
  (let [parsed (read-string val)]
    (and
      (integer? parsed)
      (<= min parsed max)
      )
    )
  )

(defn valid-byr? [val] (integer-between? val 1920 2002))

(defn valid-iyr? [val] (integer-between? val 2010 2020))

(defn valid-eyr? [val] (integer-between? val 2020 2030))

(defn valid-hgt?
  [val]
  (let [[match nbr unit] (re-find #"^(\d+)(cm|in)$" val)]
    (and
      (not (nil? match))
      (let [parsed (read-string nbr)]
        (case unit
          "cm" (<= 150 parsed 193)
          "in" (<= 59 parsed 76)
          false
          )
        )
      )
    )
  )

(defn valid-hcl? [val] (not (nil? (re-find #"^#[a-f0-9]{6}$" val))))

(defn valid-ecl?
  [val]
  (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} val)
  )

(defn valid-pid? [val] (not (nil? (re-find #"^[0-9]{9}$" val))))

(defn valid-field?
  [[key val]]
  (cond
    (= key :byr) (valid-byr? val)
    (= key :iyr) (valid-iyr? val)
    (= key :eyr) (valid-eyr? val)
    (= key :hgt) (valid-hgt? val)
    (= key :hcl) (valid-hcl? val)
    (= key :ecl) (valid-ecl? val)
    (= key :pid) (valid-pid? val)
    (= key :cid) true
    )
  )

(defn valid-2?
  [passport]
  (and
    (valid-1? passport)
    (every? true? (map valid-field? passport))
    )
  )

(->> input
     (map valid-2?)
     (filter true?)
     (count)
     )
