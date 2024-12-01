(ns advent.puzzle
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as sets]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [instaparse.core :as insta]))


(defn in-loc [year day]
  (io/resource (format "%d/%d.txt" year day)))


(defn in-lines
  ([year day] (in-lines identity year day))
  ([xd year day]
   (with-open [reader (io/reader (in-loc year day))]
     (into [] xd (line-seq reader)))))


(comment
  (in-lines (take 2) 2023 1))


(defn in [year day]
  (slurp (in-loc year day)))


(comment
  (in 2023 5))


(defn sample-lines [txt]
  (string/split txt #"\n"))
