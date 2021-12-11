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

(nth (seq "banana") 4)

(nth "banana" 4)

(defn transpose [a2d]
  (let [n (count (first a2d))]
    (map (fn [idx] (map (fn [row] (nth row idx)) a2d)) (range n))))

(transpose [[1 2] [3 4]])
;; => ((1 3) (2 4))

(transpose ["ab" "cd"])
;; => ((\a \c) (\b \d))

(def sample-3 (string/split-lines "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"))

(def in-3 (string/split-lines (puzzle-in 3)))

(defn power-consumption [input]
  (let [n (count input)
        t-pose (transpose input)
        freqs (map frequencies t-pose)
        highest-digits (apply str (map (fn [counts] (if (< (counts \0) (/ n 2)) 1 0)) freqs))
        gamma (Integer/parseInt highest-digits 2)
        epsilon (bit-xor (Integer/parseInt (apply str (repeat (count (first input)) \1)) 2) gamma)
        ]
    {:t-pose t-pose
     :freqs freqs
     :highest-digits highest-digits
     :gamma gamma
     :epsilon epsilon}
    (* gamma epsilon)
    ))

(power-consumption sample-3)
;; => 198

(power-consumption in-3)
;; => 3320834

(defn bit-freqs [input]
  (let [n (count input)
        t-pose (transpose input)]
    (map frequencies t-pose)
    ))

(bit-freqs sample-3)
;; => ({\0 5, \1 7} {\0 7, \1 5} {\1 8, \0 4} {\0 5, \1 7} {\0 7, \1 5})

(defn o2-rating [input idx]
  (if (= 1 (count input))
    (Integer/parseInt (first input) 2)
    (let [counts (frequencies (map #(nth % idx) input))
          bit (if (<= (counts \0) (counts \1)) \1 \0)
          input' (filter (fn [line] (= bit (nth line idx))) input)]
      (recur input' (inc idx))
      )
    )
  )

(o2-rating sample-3 0)
;; => 23

(defn scrubber-rating [input idx]
  (if (= 1 (count input))
    (Integer/parseInt (first input) 2)
    (let [counts (frequencies (map #(nth % idx) input))
          bit (if (<= (counts \1) (counts \0)) \0 \1)
          input' (filter (fn [line] (= bit (nth line idx))) input)]
      (recur input' (inc idx))
      )
    ))

