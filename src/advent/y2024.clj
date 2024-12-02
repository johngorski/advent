(ns advent.y2024
  (:require
   [advent.puzzle :as puzzle]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as sets]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [instaparse.core :as insta]))


;; Day 2

(defn parse-report [line]
  (map edn/read-string (string/split line #"\s+")))

(defn all-increasing? [report]
  (apply < report))

(defn all-decreasing? [report]
  (apply > report))

(defn level-diffs [report]
  (map #(apply - %) (partition 2 1 report)))

(defn safe-diffs? [report]
  (every? #(<= 1 % 3) (map abs (level-diffs report))))

(defn safe-report? [report]
  (and (or (all-increasing? report)
           (all-decreasing? report))
       (safe-diffs? report)))

(defn solve-day-2-part-1 [lines]
  (reduce + (sequence
             (comp
              (map parse-report)
              (map #(if (safe-report? %) 1 0)))
             lines)))

;; Day 1

(defn line-pair [line]
  (map edn/read-string (string/split line #"\s+")))


(defn parse-day-1-lines [lines]
  (let [parsed (map line-pair lines)]
    [(sort (map first parsed)) (sort (map second parsed))]))


(defn distance [x y]
  (abs (- x y)))


(defn solve-day-1-pt-1 [lines]
  (let [[left right] (parse-day-1-lines lines)]
    (reduce + (map distance left right))))


(defn solve-day-1-pt-2 [lines]
  (let [[left right] (parse-day-1-lines lines)
        right-counts (frequencies right)
        sim-score (fn [x]
                    (* x (get right-counts x 0)))]
    (reduce + (map sim-score left))))
