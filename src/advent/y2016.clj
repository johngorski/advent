(ns advent.y2016
  (:require
   [advent.puzzle :as puzzle]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as sets]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [instaparse.core :as insta]))


;; Day 1

(def north [0 1])
(def south [0 -1])
(def east [1 0])
(def west [-1 0])


;; Basis
;; [ 1  0]
;; [ 0  1]


;; to right:
;; [ 0  1][x]
;; [-1  0][y]
(defn turn-right [[x y]]
  [y (- x)])


;; to left:
;; [ 0 -1][x]
;; [ 1  0][y]
(defn turn-left [[x y]]
  [(- y) x])
