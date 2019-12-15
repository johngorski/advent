(ns advent.y2019-day12
  (:require
   [advent.y2019 :as y2019]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as sets]
   [clojure.string :as string]))

(def puzzle-12
  [[8 0 8]
   [0 -5 -10]
   [16 10 -5]
   [19 -10 -7]])

(def sample-12
  [[-1 0 2]
   [2 -10 -7]
   [4 -8 8]
   [3 5 -1]])

(def v0
  [[0 0 0]
   [0 0 0]
   [0 0 0]
   [0 0 0]])

(defn axis-gravity
  "Simulated gravity for the subject planet due to its pull from the others. All inputs are scalar values for a single axis."
  [subject others]
  (- (count (filter #(< subject %) others))
     (count (filter #(< % subject) others))))

(axis-gravity 3 [5]) ;; => 1;
(axis-gravity 5 [3]) ;; => -1;

(defn planet-gravity
  "Simulated axis gravity applied to all three dimensions for a subject planet against the others"
  [subject others]
  (let [ax (axis-gravity (first subject) (map first others))
        ay (axis-gravity (second subject) (map second others))
        az (axis-gravity (last subject) (map last others))]
    [ax ay az])) ;; probably a way to write this with juxt

(defn gravity
  "The velocity change each moon experiences due to gravitational forces with the others. For each axis, the count of planets with higher coordinates minus the count of planets with lower coordinates."
  [positions]
  (map #(planet-gravity % positions) positions))

(defn add-vector [v1 v2]
  (map + v1 v2))

(defn velocity
  "The new moon velocities when gravity takes effect on each"
  [velocities gravity]
  (map add-vector velocities gravity))

(defn position
  "New positions of each moon after their velocities are applied"
  [positions velocities]
  (map add-vector positions velocities))

(defn energy [vector]
  (reduce + (map y2019/abs vector)))

(defn total-energy
  [positions velocities]
  (reduce + (map * (map energy positions) (map energy velocities))))

(defn step [[positions velocities]]
  (let [g           (gravity positions)
        velocities' (velocity velocities g)
        positions' (position positions velocities')]
    [positions' velocities']))

(step [sample-12 v0])
;; => [((2 -1 1) (3 -7 -4) (1 -7 5) (2 2 0))
;;     ((3 -1 -1) (1 3 3) (-3 1 -3) (-1 -3 1))]

(apply total-energy (nth (iterate step [sample-12 v0]) 10)) ;; => 179;

(apply total-energy (nth (iterate step [puzzle-12 v0]) 1000)) ;; => 12490;

(defn count-period [step-idx seen state]
  (if (seen state)
    step-idx
    (recur (inc step-idx) (conj seen state) (step state))))

(count-period 0 #{} [sample-12 v0]) ;; => 2772;

(comment
  (count-period 0 #{} [puzzle-12 v0])
  )
   
