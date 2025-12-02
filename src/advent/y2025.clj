(ns advent.y2025
  (:require
   ;; [advent.grid :as grids]
   ;; [advent.puzzle :as puzzle]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as sets]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [instaparse.core :as insta]
   ))

(defn in-string [day]
  (slurp (io/resource (str "2025/" day ".txt"))))

(defn in-lines [day]
  (string/split-lines (slurp (io/resource (str "2025/" day ".txt")))))

(defn in [day]
  (map edn/read-string
       (string/split-lines (slurp (io/resource (str "2025/" day ".txt"))))))

;; Day 1

(def sample-1
  (map string/trim
       (clojure.string/split-lines
        "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")))

(defn parse-rotation [line]
  (let [[dir & rest] (string/trim line)
        distance (edn/read-string (apply str rest))]
    [dir distance]))

(defn rotation-fn [rotation]
  (let [[dir distance] rotation
        operation ({\L - \R +} dir)]
    (fn [pointing-at]
      (rem
       (operation pointing-at distance)
       100))))

(defn rotation [line]
  (rotation-fn (parse-rotation line)))

(comment
  (map parse-rotation sample-1)

  (reductions
   (fn [pointing-at rot]
     (rot pointing-at))
   50
   (map rotation sample-1)))

(defn password [in-lines]
  (count
   (filter zero?
           (reductions
            (fn [pointing-at rot]
              (rot pointing-at))
            50
            (map rotation in-lines)))))

(comment
  (password sample-1)
  ;; => 3
  ()


  (map rotation (take 3 (in-lines 1)))

  (parse-rotation (first (in-lines 1)))
  (rotation (first (in-lines 1)))

  (let [[dir & rest] "R31"]
    [dir (edn/read-string (apply str rest))])

  (password (in-lines 1))
  ;; => 1154
  ())


(defn click-fns [rotation]
  (let [[dir distance] rotation
        operation ({\L - \R +} dir)
        click (fn [pointing-at]
                (rem
                 (operation pointing-at 1)
                 100))]
    (repeat distance click)))

(defn clicks [line]
  (click-fns (parse-rotation line)))


(defn password-1-2 [in-lines]
  (count
   (filter zero?
           (reductions
            (fn [pointing-at click]
              (click pointing-at))
            50
            (mapcat clicks in-lines)))))

(comment
  (password-1-2 sample-1)
  ;; => 6

  (password-1-2 (map string/trim (in-lines 1)))
  ;; => 6819
  ())


;; Day 2

(def sample-2
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")


(defn first-half [n]
  (let [s (str n)
        len (count s)]
    (and
     (zero? (rem len 2))
     (let [half-len (/ len 2)
           first-half (subs s 0 half-len)]
       (= s (str first-half first-half))))))

(defn silly?
  "made only of some sequence of digits repeated twice"
  [n]
  (let [s (str n)
        len (count s)]
    (and
     (zero? (rem len 2))
     (let [half-len (/ len 2)
           first-half (subs s 0 half-len)]
       (= s (str first-half first-half))))))


(comment
  (map silly?
       [55
        6464
        123123
        0101
        ]))


(defn parse-sample-ranges [line]
  (map (fn [low-high-inclusive]
         (let [bounds-s (string/split low-high-inclusive #"-")]
           (map edn/read-string bounds-s)))
       (string/split line #",")))

(comment
  (parse-sample-ranges sample-2))

(defn range-numbers [[low high-inclusive]]
  (range low (inc high-inclusive)))


(defn day-2-part-1 [line]
  (reduce +
          (sequence
           (comp
            (mapcat range-numbers)
            (filter silly?))
           (parse-sample-ranges line))))

(comment
  (day-2-part-1 sample-2)
  ;; => 1227775554

  (day-2-part-1 (in-string 2))
  ;; => 22062284697


  (apply max (map count (mapcat (fn [r] (map str r)) (parse-sample-ranges sample-2))))
  ;; => 10
  (apply max (map count (mapcat (fn [r] (map str r)) (parse-sample-ranges (in-string 2)))))
  ;; => 10
  ())

(defn silly-at?
  "When n is a result of r repetitions of a smaller pattern. (silly? n) is equivalent to (silly-at? n 2)."
  [n r]
  (let [s (str n)
        len (count s)]
    (and
     (zero? (rem len r))
     (let [small-len (/ len r)
           first-part (subs s 0 small-len)]
       (= s (apply str (repeat r first-part)))))))

(comment
  (map (fn [n] (silly-at? n 2))
       [55
        6464
        123123
        0101
        ]))

(defn sillier?
  "When n is the result of repeated letters"
  [n]
  (some (fn [r] (silly-at? n r))
        (range 2 (inc (count (str n))))))

(comment
  (map sillier?
       [12341234 123123123 1212121212 1111111]))

(defn day-2-part-2 [line]
  (reduce +
          (sequence
           (comp
            (mapcat range-numbers)
            (filter sillier?))
           (parse-sample-ranges line))))

(comment
  (day-2-part-2 sample-2)
  ;; => 4174379265

  (day-2-part-2 (in-string 2))
  ;; => 46666175279
  ())

;; Day 3



