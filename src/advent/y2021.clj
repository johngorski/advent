(ns advent.y2021
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn puzzle-in [day] (slurp (io/resource (str "2021/" day ".txt"))))

(def sample-1 [199
               200
               208
               210
               200
               207
               240
               269
               260
               263])

(defn num-increases [nums]
  (count (filter (fn [[a b]] (< a b)) (partition 2 1 nums))))

(comment
  (num-increases sample-1)
  (num-increases (map edn/read-string (string/split (puzzle-in 1) #"\n"))))

(defn num-window-increases [nums]
  (num-increases (map #(apply + %) (partition 3 1 nums))))

(comment
  (num-window-increases sample-1)
  )
