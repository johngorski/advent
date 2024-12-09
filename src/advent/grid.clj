(ns advent.grid
  (:require
   [clojure.string :as string]))



(defn grid [lines]
  (mapv #(string/split % #"") lines))

(defn grid-size [grid]
  {:rows (count grid)
   :columns (count (first grid))})

(defn bounds-checker [grid]
  (let [{:keys [rows columns]} (grid-size grid)]
    (fn [[row-idx col-idx]]
      (and (<= 0 row-idx (dec rows))
           (<= 0 col-idx (dec columns))))))

(def v+ (partial mapv +))

(defn cell-at [grid location]
  (let [[r c] location]
    (get-in grid [r c])))

(defn loc-seq
  [grid start direction]
  (let [in-bounds? (bounds-checker grid)
        step (fn [loc] (v+ loc direction))]
    (sequence
     (take-while in-bounds?)
     (iterate step start))))

(defn cell-seq
  "Starting from location start in grid, move in direction increments until running off
  the end of the grid."
  [grid start direction]
  (map (fn [loc]
         (cell-at grid loc))
       (loc-seq grid start direction)))

(defn cell-locs
  [grid]
  (let [{:keys [rows columns]} (grid-size grid)]
    (for [r (range rows)
          c (range columns)]
      [r c])))


(defn val-grid-locations [val grid]
  (filter (fn [loc]
            (= val (cell-at grid loc)))
          (cell-locs grid)))


