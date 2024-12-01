(ns advent.y2024
  (:require
   [advent.puzzle :as puzzle]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as sets]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [instaparse.core :as insta]))

(puzzle/in-lines 2024 1)



;; Day 1

(def sample-1
  "3   4
4   3
2   5
1   3
3   9
3   3")



