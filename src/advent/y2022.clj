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
