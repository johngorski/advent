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
    (* gamma epsilon)
    ))

(power-consumption sample-3)
;; => 198

(comment
  (power-consumption in-3)
  ;; => 3320834
  )

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
          bit (if (>= (counts \1) (counts \0)) \0 \1)
          input' (filter (fn [line] (= bit (nth line idx))) input)]
      (recur input' (inc idx))
      )
    ))

(scrubber-rating sample-3 0)
;; => 10

(defn life-support-rating [input]
  (let [o2 (o2-rating input 0)
        scrubber (scrubber-rating input 0)]
    (* o2 scrubber)))

(life-support-rating sample-3)
;; => 230

(comment
  (life-support-rating in-3)
  ;; => 4481199
  )

(def sample-4 "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(defn string->board [s]
  (let [rows (string/split s #"\n")]
    (mapv (fn [row] (mapv #(Integer/parseInt %) (remove empty? (string/split row #"\s+")))) rows)))

(let [[drawing-string & board-strings] (string/split sample-4 #"\n\n")
      drawings (map #(Integer/parseInt %) (string/split drawing-string #","))
      boards (map string->board board-strings)
      ]
  {:drawings drawings
   :boards boards})

(comment
  {:drawings (7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1),
   :boards ([[22 13 17 11 0]
             [8 2 23 4 24]
             [21 9 14 16 7]
             [6 10 3 18 5]
             [1 12 20 15 19]]
            [[3 15 0 2 22]
             [9 18 13 17 5]
             [19 8 7 25 23]
             [20 11 10 24 4]
             [14 21 16 12 6]]
            [[14 21 17 24 4]
             [10 16 15 9 19]
             [18 8 23 26 20]
             [22 11 13 6 5]
             [2 0 12 3 7]])})

;; wip - parse sample boards and drawings

(defn bingo-rows [board]
  (seq board))

(defn bingo-cols [board]
  (map (fn [col] (map (fn [row] (get row col)) board)) (range (count (first board)))))

(bingo-cols [[1 2 3] [4 5 6] [7 8 9]])
;; => ((1 4 7) (2 5 8) (3 6 9))
(bingo-rows [[1 2 3] [4 5 6] [7 8 9]])
;; => ([1 2 3] [4 5 6] [7 8 9])

(some even? [1 3 5])
;; => nil
(some even? [1 3 6])
;; => true
(some #{'a 2 'd} [1 2 3])
;; => 2
(some #(not (#{'a 2 3} %)) [1 2 3])
;; => true
(some #(not (#{'a 2 3 1} %)) [1 2 3])
;; => nil
(not (some #(not (#{'a 2 3 1} %)) [1 2 3]))
;; => true

(defn bingo? [drawn line]
  (not (some #(not (drawn %)) line)))

(bingo? #{'a 2 3 1} [1 2 3])
;; => true
(bingo? #{'a 2 3 1} [1 2 4])
;; => false

(defn wins-bingo? [drawn board]
  (when
      (or
       (some #(bingo? drawn %) (bingo-rows board))
       (some #(bingo? drawn %) (bingo-cols board))
       )
    board))

(wins-bingo? #{} [[1 2 3] [4 5 6] [7 8 9]])
;; => nil

(wins-bingo? #{1 4 7} [[1 2 3] [4 5 6] [7 8 9]])
;; => true

(wins-bingo? #{1 4 8} [[1 2 3] [4 5 6] [7 8 9]])
;; => nil

(wins-bingo? #{4 5 6} [[1 2 3] [4 5 6] [7 8 9]])
;; => true

(defn bingo-winner [drawn to-draw boards]
  (or
   (when-let [winner (some #(wins-bingo? drawn %) boards)]
     {:winner winner
      :drawn drawn
      })
   (when (not-empty to-draw)
     (recur (conj drawn (first to-draw)) (rest to-draw) boards))))

(let [[drawing-string & board-strings] (string/split sample-4 #"\n\n")
      drawings (map #(Integer/parseInt %) (string/split drawing-string #","))
      boards (map string->board board-strings)
      ]
  {:drawings drawings
   :boards boards}
  (bingo-winner #{} drawings boards))

(comment
  [[14 21 17 24 4]
   [10 16 15 9 19]
   [18 8 23 26 20]
   [22 11 13 6 5]
   [2 0 12 3 7]])

(or false nil :a)
;; => :a
(and false nil :a)
;; => false

(bingo-winner #{} [3 4 5] [[[1 2 3] [4 5 6] [7 8 9]]])
;; => nil

(bingo-winner #{} [3 4 5 6 7 8] [[[1 2 3] [4 5 6] [7 8 9]]])
;; => {:winner [[1 2 3] [4 5 6] [7 8 9]], :drawn #{4 6 3 5}}

(apply concat [[1 2] [3 4]])
;; => (1 2 3 4)

(defn bingo-score [drawn winner last-drawn]
  (let [sum (apply + (remove drawn (apply concat winner)))]
    (* sum last-drawn)))

(defn parse-bingo [input]
  (let [[drawing-string & board-strings] (string/split input #"\n\n")
        drawings (map #(Integer/parseInt %) (string/split drawing-string #","))
        boards (map string->board board-strings)
        ]
    {:drawings drawings
     :boards boards}))

(parse-bingo sample-4)

(let [{:keys [drawings boards]} (parse-bingo sample-4)
      {:keys [drawn winner]} (bingo-winner #{} drawings boards)
      last-drawn (last (filter drawn drawings))
      ]
  (bingo-score drawn winner last-drawn)
  )

(let [{:keys [drawings boards]} (parse-bingo (puzzle-in 4))
      {:keys [drawn winner]} (bingo-winner #{} drawings boards)
      last-drawn (last (filter drawn drawings))
      ]
  (bingo-score drawn winner last-drawn)
  )
;; => 39984

(comment
  (or
   (when-let [winner (some #(wins-bingo? drawn %) boards)]
     {:winner winner
      :drawn drawn
      })
   (when (not-empty to-draw)
     (recur (conj drawn (first to-draw)) (rest to-draw) boards))))

(defn last-bingo-winner [drawings drawn to-draw boards]
  (cond
    (empty? boards)
    nil

    (and
     (= 1 (count boards))
     (wins-bingo? drawn (first boards)))
    (let [last-winner (first boards)
          last-drawn (last (filter drawn drawings))]
      ;; (bingo-score drawn last-winner last-drawn)
      {:winner last-winner
       :drawn drawn})

    :else
    (recur
     drawings
     (conj drawn (first to-draw))
     (rest to-draw)
     (remove #(wins-bingo? drawn %) boards))
    ))

(let [{:keys [drawings boards]} (parse-bingo (puzzle-in 4))
      {:keys [drawn winner]} (last-bingo-winner drawings #{} drawings boards)
      last-drawn (last (filter drawn drawings))
      ]
  (bingo-score drawn winner last-drawn)
  )
;; => 8468

(def sample-5 "0,9 -> 5,9
  8,0 -> 0,8
  9,4 -> 3,4
  2,2 -> 2,1
  7,0 -> 7,4
  6,4 -> 2,0
  0,9 -> 2,9
  3,4 -> 1,4
  0,0 -> 8,8
  5,5 -> 8,2")

(defn parse-5 [in]
  (->> (string/split-lines in)
       (map #(rest (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" (string/trim %))))
       (map (fn [strings] (map #(Integer/parseInt %) strings)))
       (map (fn [[x1 y1 x2 y2]] [[x1 y1] [x2 y2]]))
       ))

(parse-5 sample-5)
;; => ([[0 9] [5 9]] [[8 0] [0 8]] [[9 4] [3 4]] [[2 2] [2 1]] [[7 0] [7 4]] [[6 4] [2 0]] [[0 9] [2 9]] [[3 4] [1 4]] [[0 0] [8 8]] [[5 5] [8 2]])

(defn horizontal? [[[x1 y1] [x2 y2]]]
  (= y1 y2))

(defn vertical? [[[x1 y1] [x2 y2]]]
  (= x1 x2))

(->> sample-5
     parse-5
     (filter #(or (horizontal? %) (vertical? %))))

(defn vertical-points [[[x1 y1] [x2 y2]]]
  (let [lo (min y1 y2)
        hi (max y1 y2)]
    (for [y (range lo (inc hi))]
      [x1 y])))

(defn horizontal-points [[[x1 y1] [x2 y2]]]
  (let [lo (min x1 x2)
        hi (max x1 x2)]
    (for [x (range lo (inc hi))]
      [x y1])))

(defn slash? [[[x1 y1] [x2 y2]]]
  (or
   (and
    (< y1 y2)
    (< x1 x2))
   (and
    (< y2 y1)
    (< x2 x1))))

(defn slash-points [[[x1 y1] [x2 y2]]]
  (let [num-points (inc (Math/abs (- x2 x1)))]
    (if (< x1 x2)
      (for [idx (range num-points)]
        [(+ x1 idx) (+ y1 idx)])
      (for [idx (range num-points)]
        [(+ x2 idx) (+ y2 idx)]))))

(defn backslash? [[[x1 y1] [x2 y2]]]
  (or
   (and
    (< x1 x2)
    (< y2 y1))
   (and
    (< x2 x1)
    (< y1 y2))))

(defn backslash-points [[[x1 y1] [x2 y2]]]
  (let [num-points (inc (Math/abs (- x2 x1)))]
    (if (< x1 x2)
      (for [idx (range num-points)]
        [(+ x1 idx) (- y1 idx)])
      (for [idx (range num-points)]
        [(+ x2 idx) (- y2 idx)]))))

(defn line->points [line]
  (cond
    (horizontal? line)
    (horizontal-points line)

    (vertical? line)
    (vertical-points line)

    (slash? line)
    (slash-points line)

    (backslash? line)
    (backslash-points line)

    :else
    []))

(line->points [[0 3] [3 0]])
;; => ([0 3] [1 2] [2 1] [3 0])

(comment (frequencies (mapcat line->points (parse-5 sample-5))))
(comment
  {[7 1] 1,
   [2 2] 1,
   [3 9] 1,
   [8 4] 1,
   [7 2] 1,
   [7 4] 2,
   [5 4] 1,
   [3 4] 2,
   [7 3] 1,
   [1 9] 2,
   [4 9] 1,
   [2 9] 2,
   [0 9] 2,
   [1 4] 1,
   [6 4] 1,
   [5 9] 1,
   [2 4] 1,
   [7 0] 1,
   [2 1] 1,
   [9 4] 1,
   [4 4] 1})

(defn overlaps [lines]
  (->>
   (frequencies (mapcat line->points lines))
   (filter (fn [[point freqs]] (< 1 freqs)))
   count
   )
  )

(overlaps (parse-5 sample-5))
;; => 5

(comment
  (overlaps (parse-5 (puzzle-in 5)))
  ;; => 6225
  ;; => 22116
  )

(def sample-6 [3,4,3,1,2])

(defn age-fish [fish]
  (if (zero? fish)
    6
    (dec fish)))

(defn fish-day [fishes]
  (let [baby-count (count (filter zero? fishes))]
    (concat (map age-fish fishes) (repeat baby-count 8))))

(fish-day sample-6)
;; => (2 3 2 0 1)

(take 19 (iterate fish-day sample-6))
(comment
  ([3 4 3 1 2]
   (2 3 2 0 1)
   (1 2 1 6 0 8)
   (0 1 0 5 6 7 8)
   (6 0 6 4 5 6 7 8 8)
   (5 6 5 3 4 5 6 7 7 8)
   (4 5 4 2 3 4 5 6 6 7)
   (3 4 3 1 2 3 4 5 5 6)
   (2 3 2 0 1 2 3 4 4 5)
   (1 2 1 6 0 1 2 3 3 4 8)
   (0 1 0 5 6 0 1 2 2 3 7 8)
   (6 0 6 4 5 6 0 1 1 2 6 7 8 8 8)
   (5 6 5 3 4 5 6 0 0 1 5 6 7 7 7 8 8)
   (4 5 4 2 3 4 5 6 6 0 4 5 6 6 6 7 7 8 8)
   (3 4 3 1 2 3 4 5 5 6 3 4 5 5 5 6 6 7 7 8)
   (2 3 2 0 1 2 3 4 4 5 2 3 4 4 4 5 5 6 6 7)
   (1 2 1 6 0 1 2 3 3 4 1 2 3 3 3 4 4 5 5 6 8)
   (0 1 0 5 6 0 1 2 2 3 0 1 2 2 2 3 3 4 4 5 7 8)
   (6 0 6 4 5 6 0 1 1 2 6 0 1 1 1 2 2 3 3 4 6 7 8 8 8 8)
  ))

(nth (iterate fish-day sample-6) 18)
;; => (6 0 6 4 5 6 0 1 1 2 6 0 1 1 1 2 2 3 3 4 6 7 8 8 8 8)

(count (nth (iterate fish-day sample-6) 18))
;; => 26

(count (nth (iterate fish-day sample-6) 80))
;; => 5934

(count (nth (iterate fish-day sample-6) 80))

(map #(Integer/parseInt (string/trim %)) (string/split (puzzle-in 6) #","))

;; (commment
;; (count (nth (iterate fish-day (map #(Integer/parseInt (string/trim %)) (string/split (puzzle-in 6) #","))) 80))
;; => 353079
;; )

(comment
  ;; too slow for day 6 part 2 (not surprising)
  (count (nth (iterate fish-day (map #(Integer/parseInt (string/trim %)) (string/split (puzzle-in 6) #","))) 256))
  )

;; Skip ahead to day 7

;; Minimum crab fuel seems like it would be a value at the median.
;; If you sort the crabs, keep removing the outermost crabs until we're at the middle.
;; Anywhere inside will do, and anywhere inside will have the same fuel consumptin.

(def sample-7 [16,1,2,0,4,2,7,1,2,14])

(defn fuel-consumption [position crabs]
  (reduce + (map #(Math/abs (- % position)) crabs)))

(fuel-consumption 2 sample-7)

(defn median [nums]
  (let [N (count nums)
        sorted (sort nums)]
    (nth sorted (Math/floor (/ N 2)))))

(median sample-7)
;; => 2

(defn min-crab-fuel [crabs]
  (fuel-consumption (median crabs) crabs))

(min-crab-fuel sample-7)
;; => 37

