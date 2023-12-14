(ns advent.y2023
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as sets]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [instaparse.core :as insta]))

(defn in [day]
  (map edn/read-string
       (string/split-lines (slurp (io/resource (str "2023/" day ".txt"))))))

(defn in-lines [day]
  (string/split-lines (slurp (io/resource (str "2023/" day ".txt")))))

;; Day 4

(def sample-4 (string/split "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11" #"\n"))

(defn winning-numbers [card-line]
  (into #{}
        (map edn/read-string)
        (-> card-line
            (string/split #":")
            second
            (string/split #"\|")
            first
            string/trim
            (string/split #"\s+")
            )))

(comment
  (winning-numbers (first sample-4))
  ;; => #{86 48 41 17 83}
  )

(defn numbers-you-have [card-line]
  (into #{}
        (map edn/read-string)
        (-> (string/split card-line #"\|")
            second
            string/trim
            (string/split #"\s+"))))

(comment
  (numbers-you-have (first sample-4))
  ;; => #{86 48 31 6 17 9 83 53}
  )

(defn winning-numbers-you-have [card-line]
  (count (sets/intersection (winning-numbers card-line)
                            (numbers-you-have card-line))))

(defn card-points [card-line]
  (let [match-count (winning-numbers-you-have card-line)]
    (if (zero? match-count)
      0
      (int (Math/pow 2 (dec match-count))))))

(comment
  (card-points (first sample-4))
  ;; => 8
  (map card-points sample-4)
  ;; => (8 2 2 1 0 0)
  (reduce + (map card-points sample-4))
  ;; => 13
  (reduce + (map card-points (in-lines 4)))
  ;; => 23750
  )

;; Day 1
(def sample-1 (string/split "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet" #"\n"))
(comment
  (in-lines 1))

(defn line-digits [line]
  (filter (set (map str (range 10))) (string/split line #"")))

(defn calibration-value [line]
  (let [digits (line-digits line)]
    (edn/read-string (apply str ((juxt first last) digits)))))

(comment
  (calibration-value (first sample-1)))

(defn d1p1 [lines]
  (reduce + (map calibration-value lines)))

(comment
  (d1p1 sample-1)
  ;; => 142

  (d1p1 (in-lines 1))
  ;; => 56506
  )

(def digit-regex
  #"(\d|one|two|three|four|five|six|seven|eight|nine)")

(defn digits [line]
  (map second ((juxt first last) (re-seq digit-regex line))))

(defn replace-digits [line]
  (-> line
      (string/replace "one" "1")
      (string/replace "two" "2")
      (string/replace "three" "3")
      (string/replace "four" "4")
      (string/replace "five" "5")
      (string/replace "six" "6")
      (string/replace "seven" "7")
      (string/replace "eight" "8")
      (string/replace "nine" "9")
      ))

(defn string-calibration-value [line]
  (edn/read-string (apply str (map replace-digits (digits line)))))

(def sample-1-2 (string/split "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen" #"\n"))

(comment
  (map string-calibration-value sample-1-2))
;; => (29 83 13 24 42 14 76)

(defn d1p2 [lines]
  (reduce + (map string-calibration-value lines)))

(comment
  (d1p2 sample-1-2)
  ;; => 281

  (d1p2 (in-lines 1))
  ;; => 56001
  )

