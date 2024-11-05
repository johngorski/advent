(ns advent.y2023
  (:require
   [advent.puzzle :as puzzle]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as sets]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [instaparse.core :as insta]))

(defn in [day]
  (map edn/read-string
       (string/split-lines (slurp (io/resource (str "2023/" day ".txt"))))))

(puzzle/in-lines 2023 1)

;; Day 4

(def sample-4 (string/split "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11" #"\n"))

(defn winning-numbers [card-line]
  (into #{}
        (map edn/read-string)
        (-> card-line
            (string/split #":")
            second
            (string/split #"\|")
            first
            string/trim
            (string/split #"\s+")
            )))

(comment
  (winning-numbers (first sample-4))
  ;; => #{86 48 41 17 83}
  )

(defn numbers-you-have [card-line]
  (into #{}
        (map edn/read-string)
        (-> (string/split card-line #"\|")
            second
            string/trim
            (string/split #"\s+"))))

(comment
  (numbers-you-have (first sample-4))
  ;; => #{86 48 31 6 17 9 83 53}
  )

(defn winning-numbers-you-have [card-line]
  (count (sets/intersection (winning-numbers card-line)
                            (numbers-you-have card-line))))

(defn card-points [card-line]
  (let [match-count (winning-numbers-you-have card-line)]
    (if (zero? match-count)
      0
      (int (Math/pow 2 (dec match-count))))))

(comment
  (card-points (first sample-4))
  ;; => 8
  (map card-points sample-4)
  ;; => (8 2 2 1 0 0)
  (reduce + (map card-points sample-4))
  ;; => 13
  (reduce + (map card-points (puzzle/in-lines 2023 4)))
  ;; => 23750
  )

;; Day 3

(def sample-3 (string/split "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.." #"\n"))

;; get number locations (row + column range)
;; check adjacent box for non-period/non-number symbols

;; scan through each cell and record number locations

(def digits (into #{} (seq "1234567890")))

(defn number-partitions [line]
  (partition-by #(not (not (digits %))) line))

(comment
  (number-partitions "467..114..")
  ((\4 \6 \7) (\. \.) (\1 \1 \4) (\. \.)))

(defn digit-partition? [part]
  (digits (first part)))

(defn partition-columns [acc idx partitions]
  (let [[first-partition & remaining-partitions] partitions
        next-idx (+ idx (count first-partition))]
    (cond
      (empty? partitions)
      acc

      (digit-partition? first-partition)
      (recur (conj acc [idx next-idx]) next-idx remaining-partitions)

      :else
      (recur acc next-idx remaining-partitions)
      )))

(defn number-columns [line]
  (partition-columns [] 0 (number-partitions line)))

(comment
  (number-columns "467..114..")
  [[0 3] [5 8]]
  (number-columns "..467..114..")
  ())

(defn number-locations [lines]
  (apply concat
         (map-indexed
          (fn [row line]
            (map (fn [cols]
                   [row cols])
                 (number-columns line)))
          lines)))

(comment
  (number-locations sample-3)
  ([0 [0 3]] [0 [5 8]]
   [2 [2 4]] [2 [6 9]]
   [4 [0 3]]
   [5 [7 9]]
   [6 [2 5]]
   [7 [6 9]]
   [9 [1 4]] [9 [5 8]]))

;; HERE: number locations, next step is to check each for adjacent symbols
(defn surrounding-number [field number-location]
  (let [min-row 0
        max-row (count field)
        min-col 0
        max-col (count (first field))

        [number-row [number-start-col number-end-col]] number-location
        start-row (max min-row (dec number-row))
        end-row (min (+ 2 number-row) max-row)
        start-col (max min-col (dec number-start-col))
        end-col (min (inc number-end-col) max-col)]

    (map (fn [row]
           (subs row start-col end-col))
         (subvec field start-row end-row))))

(comment
  (surrounding-number sample-3 (first (number-locations sample-3)))
  #_("467."
     "...*")

  (surrounding-number sample-3 (second (number-locations sample-3)))
  #_(".114."
     ".....")

  (surrounding-number sample-3 (nth (number-locations sample-3) 2))
  #_("..*."
     ".35."
     "...."))

(defn number-at [field number-location]
  (let [[number-row [number-start-col number-end-col]] number-location]
    (edn/read-string (-> field (get number-row) (subs number-start-col number-end-col)))))

(comment
  (number-at sample-3 (nth (number-locations sample-3) 0))
  ;; => 467
  (number-at sample-3 (nth (number-locations sample-3) 1))
  ;; => 114
  (number-at sample-3 (nth (number-locations sample-3) 2))
  ;; => 35
  ())

(defn symbols-in [subfield]
  (sets/difference
   (into #{} (mapcat seq subfield))
   (into #{} (seq ".1234567890"))))

(symbols-in ["ab9*" "+d4e"])

(defn part-number-sum [field]
  (reduce
   +
   (sequence
    (comp
     (remove (fn [loc]
               (empty? (symbols-in (surrounding-number field loc)))))
     (map (fn [loc]
            (number-at field loc))))
    (number-locations field))))

(comment
  (part-number-sum sample-3)
  ;; => 4361

  (part-number-sum (puzzle/in-lines 2023 3))
  ;; => 540131
  ())

;; day 3 part 2
;; plan of attack: get asterisks
;; get labels on asterisks
;;  - actually easier to get which ones are gears first
;; filter asterisks with exactly two labels
;;  - and expand to labels then
;; and multiply
;; and sum

;; alternative:
;; - grab all the numbers/labels
;; - get all the surrounding asterisk locations
;; - combine based on asterisk locations

(defn surrounding-indices [field number-loc]
  (let [[row [col0 num-col-stop]] number-loc

        max-idx (fn [arr] (dec (count arr)))

        max-row-idx (max-idx field)
        max-col-idx (max-idx (first field))

        first-row-idx (max 0 (dec row))
        last-row-idx (min max-row-idx (inc row))

        first-col-idx (max 0 (dec col0))
        last-col-idx (min max-col-idx num-col-stop)
        ]
    (for [r (range first-row-idx (inc last-row-idx))
          c (range first-col-idx (inc last-col-idx))
          :when (or (not= row r)
                    (= c first-col-idx)
                    (= c last-col-idx))]
      [r c]
      )))


(defn asterisks-around [field number-loc]
  (sequence
   (filter (fn [idx]
             (= \* (get-in field idx))))
   (surrounding-indices field number-loc)))

(comment
  (mapcat (fn [loc]
            (asterisks-around sample-3 loc))
          (number-locations sample-3))
  #_([1 3] [1 3] [4 3] [8 5] [8 5]))

(defn gears [field]
  (let [number-locs (number-locations field)
        asterisks (mapcat (fn [number-loc]
                            (let [number (number-at field number-loc)
                                  asterisk-locs (asterisks-around field number-loc)]
                              (map (fn [asterisk-loc] {asterisk-loc [number]}) asterisk-locs)))
                          number-locs)
        gear-pairs (apply merge-with concat asterisks)]
    (into {} (filter (fn [pair] (= 2 (count (second pair))))) (seq gear-pairs))
    #_(seq gear-pairs)))

(comment
  (gears sample-3)
  ;; => {[1 3] (467 35), [8 5] (755 598)}
  ())

(comment
  (reduce +
          (map (fn [factors] (apply * factors)) (vals (gears sample-3))))
  ;; => 467835
  ())

(comment
  (reduce +
          (map (fn [factors] (apply * factors)) (vals (gears (puzzle/in-lines 2023 3)))))
  ;; => 86879020
  ())


;; Day 2

(defn game-id [line]
  (-> line
      (string/split #":")
      first
      (string/split #" ")
      second
      edn/read-string))

(comment
  (game-id "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")
  ;; => 2
  )

(defn game-handful [handful-str]
  (into {}
        (comp
         (map string/trim)
         (map (fn [entry] (string/split entry #"\s+")))
         (map (fn [[amount color]] [color (edn/read-string amount)]))
         )
        (string/split handful-str #",")))

(comment
  (game-handful " 3 green, 4 blue, 1 red")
  ;; => {"green" 3, "blue" 4, "red" 1}
  )

(defn game-handfuls [line]
  (map game-handful
       (-> line
           (string/split #":")
           second
           (string/split #";")
           )))

(comment
  (game-handfuls "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")
  ({"blue" 1, "green" 2}
   {"green" 3, "blue" 4, "red" 1}
   {"green" 1, "blue" 1})
  )

(defn game-checker [bag]
  (fn [handful]
    (->> (seq handful)
         (map (fn [[color handful-cubes]]
                (<= handful-cubes (get bag color 0))
                ))
         (reduce #(and %1 %2))
         )))

(def sample-2 (string/split "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" #"\n"))

(defn game-plausible? [line]
  (every? (fn [handful]
            ((game-checker {"red" 12, "green" 13, "blue" 14}) handful))
          (game-handfuls line)))

(comment
  (filter game-plausible? sample-2)
  ("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
   "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
   "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

(defn d2p1 [lines]
  (->> lines
       (filter game-plausible?)
       (map game-id)
       (reduce +)
       ))

(comment
  (d2p1 sample-2)
  ;; => 8
  (d2p1 (puzzle/in-lines 2023 2))
  ;; => 2278
  )

(defn fewest-cubes [handfuls]
  (apply merge-with max handfuls))

(comment
  (fewest-cubes (game-handfuls (first sample-2)))
  ;; => {"blue" 6, "red" 4, "green" 2}
  ())

(defn game-power [handfuls]
  (reduce * (vals (fewest-cubes handfuls))))

(comment
  (game-power (game-handfuls (first sample-2)))
  ;; => 48
  ())

(map (comp game-power game-handfuls) sample-2)

(def d2p2
  (reduce +
          (puzzle/in-lines
           (map (comp game-power game-handfuls))
           2023 2)))
;; => 67953

;; Day 1
(def sample-1 (string/split "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet" #"\n"))
(comment
  (puzzle/in-lines 2023 1))

(defn line-digits [line]
  (filter (set (map str (range 10))) (string/split line #"")))

(defn calibration-value [line]
  (let [digits (line-digits line)]
    (edn/read-string (apply str ((juxt first last) digits)))))

(comment
  (calibration-value (first sample-1)))

(defn d1p1 [lines]
  (reduce + (map calibration-value lines)))

(comment
  (d1p1 sample-1)
  ;; => 142

  (d1p1 (puzzle/in-lines 2023 1))
  ;; => 56506
  )

(def digit-regex
  #"(\d|one|two|three|four|five|six|seven|eight|nine)")

#_(defn digits [line]
  (map second ((juxt first last) (re-seq digit-regex line))))

(comment
  (digits "pqr3stu8vwx")
  ;; => ("3" "8")
  ())

(defn replace-digits [line]
  (-> line
      (string/replace "one" "1")
      (string/replace "two" "2")
      (string/replace "three" "3")
      (string/replace "four" "4")
      (string/replace "five" "5")
      (string/replace "six" "6")
      (string/replace "seven" "7")
      (string/replace "eight" "8")
      (string/replace "nine" "9")
      ))

(defn string-calibration-value [line]
  (edn/read-string (apply str (map replace-digits (digits line)))))

(string-calibration-value "7pqrstsixteen")
;; => 76

(defn spelled-calibration-value [line]
   (calibration-value (replace-digits line)))

(spelled-calibration-value "7pqrstsixteen")

(def sample-1-2 (string/split "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen" #"\n"))

(comment
  (map string-calibration-value sample-1-2))
;; => (29 83 13 24 42 14 76)

(defn d1p2 [lines]
  (reduce + (map string-calibration-value lines)))

(defn day1part2 [lines]
  (reduce + (map spelled-calibration-value lines)))

(comment
  (d1p2 sample-1-2)
  ;; => 281
  (day1part2 sample-1-2)

  (d1p2 sample-1)

  (puzzle/in-lines (take 5) 2023 1)

  (d1p2 (puzzle/in-lines 2023 1))
  ;; => 56001
  ;; The website reports this is too low.
  )

;; This is looking like a surprisingly bespoke bit of Clojure/Java.
(def reddit-cases
  ["one"
   "two"
   "three"
   "four"
   "five"
   "six"
   "seven"
   "eight"
   "nine"
   "oneight"
   "twone"
   "threeight"
   "fiveight"
   "sevenine"
   "eightwo"
   "eighthree"
   "nineight"])

