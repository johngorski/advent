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
  (num-window-increases (map edn/read-string (string/split (puzzle-in 1) #"\n"))))

(def sub-commands
  {"forward"
   (fn [x]
     (fn [[horizontal-position depth]]
       [(+ x horizontal-position) depth]))

   "down"
   (fn [x]
     (fn [[horizontal-position depth]]
       [horizontal-position (+ x depth)]))

   "up"
   (fn [x]
     (fn [[horizontal-position depth]]
       [horizontal-position (- depth x)]))})

(def sample-2
  "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(defn final-sub-position [commands]
  (reduce
   #(%2 %1)
   [0 0]
   (map
    (fn [line]
      (let [[command x] (string/split line #" ")]
        ((sub-commands command) (edn/read-string x))))
    commands)))

(final-sub-position (string/split sample-2 #"\n"))

(apply * (final-sub-position (string/split (puzzle-in 2) #"\n")))
;; => 2272262

(def aim-sub-commands
  {"forward"
   (fn [x]
     (fn [[horizontal-position depth aim]]
       [(+ x horizontal-position) (+ depth (* aim x)) aim]))

   "down"
   (fn [x]
     (fn [[horizontal-position depth aim]]
       [horizontal-position depth (+ x aim)]))

   "up"
   (fn [x]
     (fn [[horizontal-position depth aim]]
       [horizontal-position depth (- aim x)]))})

(defn aimed-sub-position [commands]
  (reduce
   #(%2 %1)
   [0 0 0]
   (map
    (fn [line]
      (let [[command x] (string/split line #" ")]
        ((aim-sub-commands command) (edn/read-string x))))
    commands)))

(aimed-sub-position (string/split sample-2 #"\n"))
;; => [15 60 10]

(let [[h d] (aimed-sub-position (string/split (puzzle-in 2) #"\n"))] (* h d))
;; => 2134882034


