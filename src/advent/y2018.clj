(ns advent.y2018
  (:require
   [advent.puzzle :as puzzle]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as sets]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [instaparse.core :as insta]))


;; Day 1

(defn solve-day-1-part-1 [nums]
  (reduce + nums))


(defn solve-day-1-part-2
  ([nums] (solve-day-1-part-2 #{0} 0 (cycle nums)))
  ([freqs freq nums]
   (let [change (first nums)
         freq' (+ change freq)]
     (if (freqs freq')
       freq'
       (recur (conj freqs freq') freq' (rest nums))))
   ))
