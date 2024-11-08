
(ns advent.core
  (:require [clojure.edn :as edn]))

(defn diff-circ-sum
  [[curr & rest :as list] last acc]
  (if (empty? list)
    acc
    (recur rest curr (if (= last curr)
                       (+ curr acc)
                       acc))))

(edn/read-string "4")
(clojure.string/split "1234" #"")

(defn wrap
  "Puts the first character back on the end."
  [s]
  (str s (first s)))

(defn nums
  "Digits in the string as ints."
  [s]
  (map edn/read-string (clojure.string/split s #"")))

(defn rev-cap
  "the sum of all digits that match the next digit in the list. The list is circular, so the digit after the last digit is the first digit in the list."
  [s]
  (->
   s
   wrap
   nums
   (diff-circ-sum nil 0)))

(defn shift
  "xs shifted over by one half of the input length"
  [xs]
  (let [half (/ (count xs) 2)]
    (concat (drop half xs) (take half xs))))

(defn circ-cap
  "Part 2 checksum."
  [s]
  (let [xs (nums s)
        shifted (shift xs)]
    (reduce + 0 (map #(if (= %1 %2) %1 0) xs shifted))))

(defn row-checksum
  [row]
  (apply - ((juxt #(apply max %) #(apply min %)) row)))

(defn ss-checksum
  [rows]
  (apply + 0 (map row-checksum rows)))

(defn row-div-check
  [row]
  (->>
   (for [a row
         b row]
     (sort [a b]))
   (remove (partial apply =))
   (filter (fn [[a b]] (= 0 (rem b a))))
   (map (fn [[a b]] (quot b a)))
   first))

(defn div-checksum
  [rows]
  (apply + 0 (map row-div-check rows)))

(defn manh-origin
  "2017 Day 3 part 1"
  [sp-idx]
  ;; Calculate bottom-right corner from squares of odd numbers
  ;; Corners are maximum-distance.
  ;; Minimum-distance are at edge midpoints.
  ;; Manhattan distance directly interpolates.
  ;; Each Manhattan ring has perimeter of (* 4 ring-index)
  ;; Range of Manhattan distance should vary with 1/8 of that.
  )
