(ns advent-of-code-2020.day-04
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-passport
  [passport]
  (into
    {}
    (map
      (fn [[_ key val]] [(keyword key) val])
      (re-seq #"([a-z]{3}):(.*?)(\s|$)" passport)
      )
    )
  )

(def input
  (map
    parse-passport
    (map
      (fn [entry] (string/replace entry "\n" " "))
      (string/split (slurp (io/resource "day-04.txt")) #"\n\n")
      )
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

(count (filter true? (map valid-1? input)))

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

(defn validate-byr [val] (integer-between? val 1920 2002))

(defn validate-iyr [val] (integer-between? val 2010 2020))

(defn validate-eyr [val] (integer-between? val 2020 2030))

(defn validate-hgt
  [val]
  (let [match (re-find #"^(\d+)(cm|in)$" val)]
    (and
      (not (nil? match))
      (let [[_ nbr unit] match
            parsed (read-string nbr)]
        (cond
          (= unit "cm") (<= 150 parsed 193)
          (= unit "in") (<= 59 parsed 76)
          )
        )
      )
    )
  )

(defn validate-hcl [val] (not (nil? (re-find #"^#[a-f0-9]{6}$" val))))

(defn validate-ecl
  [val]
  (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} val)
  )

(defn validate-pid [val] (not (nil? (re-find #"^[0-9]{9}$" val))))

(defn validate-field
  [[key val]]
  (cond
    (= key :byr) (validate-byr val)
    (= key :iyr) (validate-iyr val)
    (= key :eyr) (validate-eyr val)
    (= key :hgt) (validate-hgt val)
    (= key :hcl) (validate-hcl val)
    (= key :ecl) (validate-ecl val)
    (= key :pid) (validate-pid val)
    (= key :cid) true
    )
  )

(defn valid-2?
  [passport]
  (and
    (valid-1? passport)
    (every? true? (map validate-field passport))
    )
  )

(count (filter true? (map valid-2? input)))
