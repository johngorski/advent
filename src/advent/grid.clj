(ns advent.grid
  (:require
   [clojure.string :as string]))



(defn from-lines [lines]
  (mapv #(string/split % #"") lines))

(defn grid [lines]
  (from-lines lines))

(defn grid-size [grid]
  {:rows (count grid)
   :columns (count (first grid))})

(defn bounds-checker [grid]
  (let [{:keys [rows columns]} (grid-size grid)]
    (fn [[row-idx col-idx]]
      (and (<= 0 row-idx (dec rows))
           (<= 0 col-idx (dec columns))))))

(def v+ (partial mapv +))
(def v- (partial mapv -))

(defn cell-at [grid location]
  (let [[r c] location]
    (get-in grid [r c])))

(defn cells-at [grid locations]
  (map (fn [location]
         (cell-at grid location))
       locations))

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


(defn val-location-map
  "Map from cell value to seq of locations"
  [grid]
  (group-by #(cell-at grid %)
            (let [{:keys [rows columns]} (grid-size grid)]
              (for [r (range rows)
                    c (range columns)]
                [r c]))))


(defn mapg [f grid]
  (mapv (fn [row]
          (mapv (fn [cell]
                  (f cell))
                row))
        grid))


(defn orthogonal-locs [loc]
  [(v+ loc [0 1]) (v+ loc [1 0])
   (v- loc [0 1]) (v- loc [1 0])])


(defn orthogonal-neighbor-locs
  "seq of grid's locs orthogonal to input loc"
  [grid loc]
  (let [in-bounds? (bounds-checker grid)]
    (filter in-bounds? (orthogonal-locs loc))))


(defn adjacent-locs [loc]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not (and (zero? dx) (zero? dy))) ]
    (v+ loc [dx dy])))


(defn adjacent-neighbor-locs
  "seq of grid's locs orthogonal to input loc"
  [grid loc]
  (let [in-bounds? (bounds-checker grid)]
    (filter in-bounds? (adjacent-locs loc))))


