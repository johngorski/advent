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



