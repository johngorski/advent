(ns year-2023-day-1
  (:require
   [advent.y2023 :as y]))

;; Day 1 sample
y/sample-1

;; Mapping to calibration values
(map y/calibration-value y/sample-1)

(map y/spelled-calibration-value y/sample-1)

y/sample-1-2

(map y/string-calibration-value y/sample-1-2)

(map y/spelled-calibration-value y/sample-1-2)

(map y/digits y/sample-1-2)

(map y/digits (identity y/reddit-cases))
;; => (("one" "one") ("two" "two") ("three" "three") ("four" "four") ("five" "five") ("six" "six") ("seven" "seven") ("eight" "eight") ("nine" "nine") ("one" "one") ("two" "two") ("three" "three") ("five" "five") ("seven" "seven") ("eight" "eight") ("eight" "eight") ("nine" "nine"))

(map (fn [line] ((juxt first last) (re-seq y/digit-regex line))) y/reddit-cases)
;; => ([["one" "one"] ["one" "one"]] [["two" "two"] ["two" "two"]] [["three" "three"] ["three" "three"]] [["four" "four"] ["four" "four"]] [["five" "five"] ["five" "five"]] [["six" "six"] ["six" "six"]] [["seven" "seven"] ["seven" "seven"]] [["eight" "eight"] ["eight" "eight"]] [["nine" "nine"] ["nine" "nine"]] [["one" "one"] ["one" "one"]] [["two" "two"] ["two" "two"]] [["three" "three"] ["three" "three"]] [["five" "five"] ["five" "five"]] [["seven" "seven"] ["seven" "seven"]] [["eight" "eight"] ["eight" "eight"]] [["eight" "eight"] ["eight" "eight"]] [["nine" "nine"] ["nine" "nine"]])

(y/digits "one")
;; => ("one" "one")

(map y/digits y/reddit-cases)
;; => (("one" "one") ("two" "two") ("three" "three") ("four" "four") ("five" "five") ("six" "six") ("seven" "seven") ("eight" "eight") ("nine" "nine") ("one" "one") ("two" "two") ("three" "three") ("five" "five") ("seven" "seven") ("eight" "eight") ("eight" "eight") ("nine" "nine"))

(re-seq y/digit-regex "sevenine")

(re-find y/digit-regex "sevenine")

(re-matches y/digit-regex "sevenine")
