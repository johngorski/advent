(ns advent.y2022
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as sets]
   [clojure.string :as string]))

(defn in [day]
  (map edn/read-string
       (string/split (slurp (io/resource (str "2022/" day ".txt"))) #"\n")))



;; Day 1
(comment

  (apply max (filter some? (map #(apply + %) (partition-by some? (in 1)))))
  ;; => 71502

  (apply + (take 3 (reverse (sort (filter some? (map #(apply + %) (partition-by some? (in 1)))))))))
  ;; => 208191
