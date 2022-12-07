(ns advent.y2022
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as sets]
   [clojure.string :as string]))

(defn in [day]
  (map edn/read-string
       (string/split-lines (slurp (io/resource (str "2022/" day ".txt"))))))

(defn in-lines [day]
  (string/split-lines (slurp (io/resource (str "2022/" day ".txt")))))

;; Day 5

(defn stack [s]
  (reduce conj () (reverse (map str s))))

(comment
  {"1" (stack "QGPRLCTF")
   "2" (stack "JSFRWHQN")
   "3" (stack "QMPWHBF")
   "4" (stack "FDTSV")
   "5" (stack "ZFVWDLQ")
   "6" (stack "SLCZ")
   "7" (stack "FDVMBZ")
   "8" (stack "BJT")
   "9" (stack "HPSLGBNQ")}

  (first (drop 10 (in-lines 5))))
;; => "move 1 from 8 to 1"

(defn stack-step [s]
  (let [[_ rep-str from to]
        (re-matches #"move (\d+) from (\d+) to (\d+)" s)]
    (repeat (edn/read-string rep-str) (fn [stacks]
                                        (let [crate (first (stacks from))]
                                          (-> stacks
                                              (update from rest)
                                              (update to conj crate)))))))

(comment
  (let [stacks {"1" (stack "NZ")
                "2" (stack "DCM")
                "3" (stack "P")}
        steps ["move 1 from 2 to 1"]]
    (reduce #(%2 %1) stacks (mapcat stack-step steps)))
  ;; => {"1" ("D" "N" "Z"), "2" ("C" "M"), "3" ("P")}

  (let [stacks {"1" (stack "NZ")
                "2" (stack "DCM")
                "3" (stack "P")}
        steps ["move 1 from 2 to 1"
               "move 3 from 1 to 3"]]
    (reduce #(%2 %1) stacks (mapcat stack-step steps)))
  ;; => {"1" (), "2" ("C" "M"), "3" ("Z" "N" "D" "P")}

  (let [stacks {"1" (stack "NZ")
                "2" (stack "DCM")
                "3" (stack "P")}
        steps ["move 1 from 2 to 1"
               "move 3 from 1 to 3"
               "move 2 from 2 to 1"]]
    (reduce #(%2 %1) stacks (mapcat stack-step steps)))
  ;; => {"1" ("M" "C"), "2" (), "3" ("Z" "N" "D" "P")}

  (let [stacks {"1" (stack "NZ")
                "2" (stack "DCM")
                "3" (stack "P")}
        steps ["move 1 from 2 to 1"
               "move 3 from 1 to 3"
               "move 2 from 2 to 1"
               "move 1 from 1 to 2"]]
    (reduce #(%2 %1) stacks (mapcat stack-step steps))))
;; => {"1" ("C"), "2" ("M"), "3" ("Z" "N" "D" "P")}

(defn move-crates [stacks procedure]
  (reduce #(%2 %1) stacks (mapcat stack-step procedure)))

(comment
  (move-crates
   {"1" (stack "NZ")
    "2" (stack "DCM")
    "3" (stack "P")}
   ["move 1 from 2 to 1"
    "move 3 from 1 to 3"
    "move 2 from 2 to 1"
    "move 1 from 1 to 2"])
  ;; => {"1" ("C"), "2" ("M"), "3" ("Z" "N" "D" "P")}

  (let [final-crates
        (move-crates
         {"1" (stack "QGPRLCTF")
          "2" (stack "JSFRWHQN")
          "3" (stack "QMPWHBF")
          "4" (stack "FDTSV")
          "5" (stack "ZFVWDLQ")
          "6" (stack "SLCZ")
          "7" (stack "FDVMBZ")
          "8" (stack "BJT")
          "9" (stack "HPSLGBNQ")}
         (drop 10 (in-lines 5)))]
    (apply str (map (comp first final-crates str inc) (range 9)))))
;; => "VGBBJCRMN"

(defn stack-9001 [s]
  (into [] (map str) s))

;; (stack-9001 "abc")
;; => ["a" "b" "c"]

(defn stack-step-9001 [s]
  (let [[_ rep-str from to]
        (re-matches #"move (\d+) from (\d+) to (\d+)" s)
        crates (edn/read-string rep-str)]
    (fn [stacks]
      (let [substack (take crates (stacks from))]
        (-> stacks
            (update from #(drop crates %))
            (update to #(concat substack %)))))))

(defn move-crates-9001 [stacks procedure]
  (reduce #(%2 %1) stacks (map stack-step-9001 procedure)))

(comment
  (let [final-crates
        (move-crates-9001
         {"1" (stack "QGPRLCTF")
          "2" (stack "JSFRWHQN")
          "3" (stack "QMPWHBF")
          "4" (stack "FDTSV")
          "5" (stack "ZFVWDLQ")
          "6" (stack "SLCZ")
          "7" (stack "FDVMBZ")
          "8" (stack "BJT")
          "9" (stack "HPSLGBNQ")}
         (drop 10 (in-lines 5)))]
    (apply str (map (comp first final-crates str inc) (range 9)))))
  ;; => "LBBVJBRMH"

;; Day 4

(defn assignment [s]
  (map edn/read-string (string/split s #"-")))

;; (assignment "2-4")
;; => (2 4)

(defn assignment-pair [s]
  (map assignment (string/split s #",")))

;; (assignment-pair "2-4,6-8")
;; => ((2 4) (6 8))

(defn fully-overlaps? [left right]
  (let [[l-bottom l-top] left
        [r-bottom r-top] right]
    (cond
      (= l-bottom r-bottom) true
      (< r-bottom l-bottom) (recur right left)
      :else (<= r-top l-top))))

;; (fully-overlaps? [0 5] [1 3])
;; => true
;; (fully-overlaps? [0 3] [1 5])
;; => false

;; (count (filter #(apply fully-overlaps? %) (map assignment-pair (in-lines 4))))
;; => 571

(defn overlaps? [left right]
  (let [[l-bottom l-top] left
        [r-bottom r-top] right]
    (cond
      (= l-bottom r-bottom) true
      (< r-bottom l-bottom) (recur right left)
      :else (<= r-bottom l-top))))

;; (overlaps? [5 7] [7 9])
;; => true
;; (overlaps? [5 7] [8 9])
;; => false

;; (count (filter #(apply overlaps? %) (map assignment-pair (in-lines 4))))
;; => 917

;; Day 3

;; Find the item type that appears in both compartments of each rucksack.
;; What is the sum of the priorities of those item types?

(defn both-compartments-item [rucksack]
  (let [[left right] (partition (/ (count rucksack) 2) rucksack)]
    (first (sets/intersection (set left) (set right)))))
;; => #'advent.y2022/both-compartments-item

;; (both-compartments-item "vJrwpWtwJgWrhcsFMMfFFhFp")
;; => \p

(defn item-priority [item]
  (let [A (int \A)
        Z (int \Z)
        a (int \a)
        z (int \z)
        i (int item)]
    (cond
      (<= a i z)
      (inc (- (int item) (int \a)))

      (<= A i Z)
      (+ 27 (- (int item) (int \A)))
      )))

(comment
  (item-priority \a)
  ;; => 1
  (item-priority \z)
  ;; => 26
  (item-priority \A)
  ;; => 27

  (reduce + (map (comp item-priority both-compartments-item) (in-lines 3))))
;; => 7793

(defn badge-item [rucksacks]
  (first (apply sets/intersection (map set rucksacks))))

(comment
  (badge-item
   ["vJrwpWtwJgWrhcsFMMfFFhFp"
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
    "PmmdzqPrVvPwwTWBwg"])
  ;; => \r

  (reduce + (map (comp item-priority badge-item) (partition 3 (in-lines 3)))))
;; => 2499

;; Day 2

(defn rps-result [them you]
  (get-in
   {:Rock {:Rock :tie
           :Paper :win
           :Scissors :loss}
    :Paper {:Rock :loss
            :Paper :tie
            :Scissors :win}
    :Scissors {:Rock :win
               :Paper :loss
               :Scissors :tie}}
   [them you]))

(comment
  (rps-result :Rock :Scissors)
  ;; => :loss
  (rps-result :Scissors :Rock))
;; => :win

(defn rps-score [them you]
  (let [result (rps-result them you)]
    (+
     ({:loss 0
       :tie 3
       :win 6}
      result)
     ({:Rock 1
       :Paper 2
       :Scissors 3}
      you))))

(comment
  (rps-score :Rock :Paper)
  ;; => 8
  (rps-score :Paper :Rock)
  ;; => 1
  (rps-score :Scissors :Scissors))
;; => 6

(defn rps-decode [key moves]
  (let [[abc xyz] moves]
    (rps-score (key abc) (key xyz))))

(defn rps-strategy-score [key rounds]
  (reduce + (map #(rps-decode key %) rounds)))

(comment
  (let [key {"A" :Rock
             "B" :Paper
             "C" :Scissors
             "X" :Rock
             "Y" :Paper
             "Z" :Scissors}]
    (map #(rps-decode key %) (map #(string/split % #"\s+") (in-lines 2)))))

(rps-strategy-score
 {"A" :Rock
  "B" :Paper
  "C" :Scissors
  "X" :Rock
  "Y" :Paper
  "Z" :Scissors}
 [["A" "Y"]
  ["B" "X"]
  ["C" "Z"]])

(comment
  (rps-strategy-score
   {"A" :Rock
    "B" :Paper
    "C" :Scissors
    "X" :Rock
    "Y" :Paper
    "Z" :Scissors}
   (map #(string/split % #"\s+") (in-lines 2)))
  ;; => 11603

  (take 3 (map #(string/split % #"\s+") (in-lines 2)))
  ;; => (["B" "Y"] ["A" "Z"] ["A" "Z"])
  (last (map #(string/split % #"\s+") (in-lines 2)))
  ;; => ["A" "Z"]
  (first (map #(string/split % #"\s+") (in-lines 2))))
;; => ["B" "Y"]

(defn rps-2-score [abc xyz]
  (get-in {
           ;; vs Rock
           "A" {"X" (+ 0 3)
                "Y" (+ 3 1)
                "Z" (+ 6 2)}
           ;; vs Paper
           "B" {"X" (+ 0 1)
                "Y" (+ 3 2)
                "Z" (+ 6 3)}
           ;; vs Scissors
           "C" {"X" (+ 0 2)
                "Y" (+ 3 3)
                "Z" (+ 6 1)}
           } [abc xyz]))

(comment
  (reduce + (map #(apply rps-2-score %) (map #(string/split % #"\s+") (in-lines 2)))))
;; => 12725

;; Day 1
(comment

  (apply max (filter some? (map #(apply + %) (partition-by some? (in 1)))))
  ;; => 71502

  (apply + (take 3 (reverse (sort (filter some? (map #(apply + %) (partition-by some? (in 1)))))))))
;; => 208191
