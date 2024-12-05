(ns advent.y2024
  (:require
   [advent.puzzle :as puzzle]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as sets]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [instaparse.core :as insta]))


;; Day 3

(def mul-re #"mul\(\d+,\d+\)")

(defn get-muls [s]
  (re-seq mul-re s))

(defn extract-mul [s]
  (when-let [match (re-matches #"mul\((\d+),(\d+)\)" s)]
    (let [[_ a-str b-str] match
          a (edn/read-string a-str)
          b (edn/read-string b-str)]
      [:mul a b])))

(defn mul [a b]
  (* a b))

(defn mull [[_ a b]]
  (mul a b))

(defn solve-day-3-part-1 [in]
  (reduce +
          (sequence
           (comp
            (map extract-mul)
            (map mull))
           (get-muls in))))

(def do-re #"do\(\)")
(def don't-re #"don't\(\)")

(defn get-ops [in]
  (map first (re-seq #"(mul\(\d+,\d+\)|do\(\)|don't\(\))" in)))

(defn extract-do [s]
  (when (re-matches do-re s) [:do]))

(defn extract-don't [s]
  (when (re-matches don't-re s) [:don't]))

(defn extract-op [s]
  (or (extract-mul s)
      (extract-do s)
      (extract-don't s)))

(defn parse-ops [in]
  (map extract-op (get-ops in)))

(defn reduce-mul [cpu [_ a b]]
  (if ((:enabled-ops cpu) :mul)
    (update cpu
            :accumulator
            #(let [product (mul a b)]
               (+ (or % 0) product)))
    cpu))

(defn reduce-do [cpu [_ a b]]
  (update cpu :enabled-ops conj :mul))

(defn reduce-don't [cpu [_ a b]]
  (update cpu :enabled-ops disj :mul))

(defn op-reducer [cpu instr]
  (case (first instr)
    :mul (reduce-mul cpu instr)
    :do (reduce-do cpu instr)
    :don't (reduce-don't cpu instr)))

(defn apply-ops [cpu ops]
  (reduce
   op-reducer
   cpu
   ops))

(defn solve-day-3-part-2 [in]
  (:accumulator
   (apply-ops
    {:accumulator 0
     :enabled-ops #{:mul}}
    (parse-ops in))))


;; Day 2

(defn parse-report [line]
  (mapv edn/read-string (string/split line #"\s+")))

(defn all-increasing? [report]
  (apply < report))

(defn all-decreasing? [report]
  (apply > report))

(defn level-diffs [report]
  (map #(apply - %) (partition 2 1 report)))

(defn safe-diffs? [report]
  (every? #(<= 1 % 3) (map abs (level-diffs report))))

(defn safe-report? [report]
  (and (or (all-increasing? report)
           (all-decreasing? report))
       (safe-diffs? report)))

(defn solve-day-2-part-1 [lines]
  (reduce + (sequence
             (comp
              (map parse-report)
              (map #(if (safe-report? %) 1 0)))
             lines)))

(defn dampened-reports [report]
  (let [len (count report)]
    (map (fn [idx]
           (let [
                 front (subvec report 0 idx)
                 back (subvec report (inc idx) len)]
             (concat front back)))
         (range len))))

(defn safe-under-dampening? [report]
  (some safe-report? (dampened-reports report)))

(defn solve-day-2-part-2 [lines]
  (reduce + (sequence
             (comp
              (map parse-report)
              (map #(if (safe-under-dampening? %) 1 0)))
             lines)))

;; Day 1

(defn line-pair [line]
  (map edn/read-string (string/split line #"\s+")))


(defn parse-day-1-lines [lines]
  (let [parsed (map line-pair lines)]
    [(sort (map first parsed)) (sort (map second parsed))]))


(defn distance [x y]
  (abs (- x y)))


(defn solve-day-1-pt-1 [lines]
  (let [[left right] (parse-day-1-lines lines)]
    (reduce + (map distance left right))))


(defn solve-day-1-pt-2 [lines]
  (let [[left right] (parse-day-1-lines lines)
        right-counts (frequencies right)
        sim-score (fn [x]
                    (* x (get right-counts x 0)))]
    (reduce + (map sim-score left))))
