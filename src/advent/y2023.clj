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

#_(puzzle/in-lines 2023 1)

;; Day 6
;; Really just needs a spreadsheet.
;; https://docs.google.com/spreadsheets/d/10laazhPV3pZCO024xfAAUlbM_soDt15CeaBeiidhhlg/edit?gid=0#gid=0

;; Day 5

(def sample-5 "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(def ag-mapper
  (insta/parser
   (string/join
    "\n"
    [
     "MAPPERS = SEEDS (<'\n\n'> MAPPER)*"
     "SEEDS = <'seeds: '> NUMBERS"
     "MAPPER = a2b (<'\n'> NUMBERS)*"
     "a2b = type <'-to-'> type <' map:'>"
     "<type> = #'\\w+'"
     "NUMBERS = NUMBER (<WS> NUMBER)*"
     "<NUMBER> = #'\\d+'"
     "WS = #'\\s+'"
     ])))
;; => #'advent.y2023/ag-mapper
;; #_(insta/parse ag-mapper "12 34 5")
;; (insta/parse ag-mapper sample-5)
(comment
  (insta/parse ag-mapper "seeds: 1 2 3")
  [:MAPPERS [:SEEDS [:NUMBERS "1" "2" "3"]]]

  (insta/parse ag-mapper "seeds: 1 2 3\n\na-to-b map:\n4 5 6")
  [:MAPPERS [:SEEDS [:NUMBERS "1" "2" "3"]] [:MAPPER [:a2b [:type "a"] [:type "b"]] [:NUMBERS "4" "5" "6"]]]

  (insta/parse ag-mapper "seeds: 1 2 3\n\na-to-b map:\n4 5 6\n\nb-to-c map:\n7 8 9")
  [:MAPPERS
   [:SEEDS [:NUMBERS "1" "2" "3"]]
   [:MAPPER [:a2b [:type "a"] [:type "b"]] [:NUMBERS "4" "5" "6"]]
   [:MAPPER [:a2b [:type "b"] [:type "c"]] [:NUMBERS "7" "8" "9"]]])

(insta/parse ag-mapper "seeds: 1 2 3\n\na-to-b map:\n4 5 6\n10 11 12\n\nb-to-c map:\n7 8 9")
[:MAPPERS
 [:SEEDS [:NUMBERS "1" "2" "3"]]
 [:MAPPER [:a2b "a" "b"] [:NUMBERS "4" "5" "6"] [:NUMBERS "10" "11" "12"]]
 [:MAPPER [:a2b "b" "c"] [:NUMBERS "7" "8" "9"]]]

(insta/parse ag-mapper "seeds: 1 2 3

a-to-bc map:
4 5 6
10 11 12

bc-to-c map:
7 8 9")
[:MAPPERS
 [:SEEDS [:NUMBERS "1" "2" "3"]]
 [:MAPPER [:a2b "a" "bc"] [:NUMBERS "4" "5" "6"] [:NUMBERS "10" "11" "12"]]
 [:MAPPER [:a2b "bc" "c"] [:NUMBERS "7" "8" "9"]]]

(insta/parse ag-mapper sample-5)
[:MAPPERS
 [:SEEDS [:NUMBERS "79" "14" "55" "13"]]
 [:MAPPER [:a2b "seed" "soil"] [:NUMBERS "50" "98" "2"] [:NUMBERS "52" "50" "48"]]
 [:MAPPER [:a2b "soil" "fertilizer"] [:NUMBERS "0" "15" "37"] [:NUMBERS "37" "52" "2"] [:NUMBERS "39" "0" "15"]]
 [:MAPPER [:a2b "fertilizer" "water"] [:NUMBERS "49" "53" "8"] [:NUMBERS "0" "11" "42"] [:NUMBERS "42" "0" "7"] [:NUMBERS "57" "7" "4"]]
 [:MAPPER [:a2b "water" "light"] [:NUMBERS "88" "18" "7"] [:NUMBERS "18" "25" "70"]]
 [:MAPPER [:a2b "light" "temperature"] [:NUMBERS "45" "77" "23"] [:NUMBERS "81" "45" "19"] [:NUMBERS "68" "64" "13"]]
 [:MAPPER [:a2b "temperature" "humidity"] [:NUMBERS "0" "69" "1"] [:NUMBERS "1" "0" "69"]]
 [:MAPPER [:a2b "humidity" "location"] [:NUMBERS "60" "56" "37"] [:NUMBERS "56" "93" "4"]]]

(defn range-data [[dest-start src-start length]]
  {:dest-start dest-start
   :src-start src-start
   :length length})

(defn range-pred [{:keys [src-start length]}]
  (fn [n]
    (and (<= src-start n)
         (< n (+ length src-start)))))

(defn range-translate [{:keys [src-start dest-start]}]
  (let [delta (- dest-start src-start)]
    (fn [n] (+ delta n))))

(comment
  ((range-translate {:src-start 98 :dest-start 50}) 99)
  ;; => 51
  ())

(defn add-mapper [ms mapper]
  (let [{:keys [from to ranges]} mapper]
    (update-in ms
               [from to]
               (fn [rs]
                 (if rs
                   (concat rs ranges)
                   ranges)))))

(comment
  (add-mapper {} '{:from "fertilizer"
                   :to "water"
                   :ranges ({:dest-start 49, :src-start 53, :length 8}
                            {:dest-start 0, :src-start 11, :length 42}
                            {:dest-start 42, :src-start 0, :length 7}
                            {:dest-start 57, :src-start 7, :length 4})}))

(add-mapper {"fertilizer" {"water" [1 2] "oil" [3 4]}}
            {:from "fertilizer"
             :to "water"
             :ranges '({:dest-start 49, :src-start 53, :length 8}
                       {:dest-start 0, :src-start 11, :length 42}
                       {:dest-start 42, :src-start 0, :length 7}
                       {:dest-start 57, :src-start 7, :length 4})})
'{"fertilizer" {"water" (1 2 {:dest-start 49, :src-start 53, :length 8} {:dest-start 0, :src-start 11, :length 42} {:dest-start 42, :src-start 0, :length 7} {:dest-start 57, :src-start 7, :length 4}),
               "oil" [3 4]}}

(def ag-transformer
  {:NUMBERS (fn [& num-strs] (map edn/read-string num-strs))
   :a2b (fn [from to] {:from from, :to to})
   :SEEDS identity
   :MAPPER (fn [{:keys [from to] :as a2b} & num-lists]
             #_(assoc a2b :ranges (map range-data num-lists))
             {from {to (map range-data num-lists)}})
   :MAPPERS (fn [seeds & mappers]
              {:seeds seeds
               :mappers (apply merge mappers) #_(mappers) #_(reduce {} add-mapper mappers)})
   })

(comment
  (assoc-in {} [:a :b] 0)
  ;; => {:a {:b 0}}
  (update-in {} [:a :b] (fn [n] (if n (inc n) 0)))
  ;; => {:a {:b 0}}
  (update-in {:a {:b 0}} [:a :b] (fn [n] (if n (inc n) 0)))
  ;; => {:a {:b 1}}
  ())

(->> sample-5
     (insta/parse ag-mapper)
     (insta/transform ag-transformer))

#_{:seeds (79 14 55 13),
   :mappers {"seed" {"soil" ({:dest-start 50, :src-start 98, :length 2}
                             {:dest-start 52, :src-start 50, :length 48})},
             "soil" {"fertilizer" ({:dest-start 0, :src-start 15, :length 37}
                                   {:dest-start 37, :src-start 52, :length 2}
                                   {:dest-start 39, :src-start 0, :length 15})},
             "fertilizer" {"water" ({:dest-start 49, :src-start 53, :length 8}
                                    {:dest-start 0, :src-start 11, :length 42}
                                    {:dest-start 42, :src-start 0, :length 7}
                                    {:dest-start 57, :src-start 7, :length 4})},
             "water" {"light" ({:dest-start 88, :src-start 18, :length 7}
                               {:dest-start 18, :src-start 25, :length 70})},
             "light" {"temperature" ({:dest-start 45, :src-start 77, :length 23}
                                     {:dest-start 81, :src-start 45, :length 19}
                                     {:dest-start 68, :src-start 64, :length 13})},
             "temperature" {"humidity" ({:dest-start 0, :src-start 69, :length 1}
                                        {:dest-start 1, :src-start 0, :length 69})},
             "humidity" {"location" ({:dest-start 60, :src-start 56, :length 37}
                                     {:dest-start 56, :src-start 93, :length 4})}}}

(defn converter [ranges]
  (fn [n]
    (loop [[r & rs] ranges]
      (if (not r)
        n
        (if ((range-pred r) n)
          ((range-translate r) n)
          (recur rs)
          )))))

(comment
  (map
   (converter '({:dest-start 50, :src-start 98, :length 2}
                {:dest-start 52, :src-start 50, :length 48}))
   '(79 14 55 13))
  ;; => (81 14 57 13)
  ())


(defn ag-parse [source]
  (->> source
       (insta/parse ag-mapper)
       (insta/transform ag-transformer)))

#_(ag-parse sample-5)

(defn next-category [ag-data current-category]
  (-> ag-data (get-in [:mappers current-category]) keys first))

(comment
  (next-category (ag-parse sample-5) "seed")
  ;; => "soil"
  (next-category (ag-parse sample-5) "soil")
  ;; => "fertilizer"
  (next-category (ag-parse sample-5) "zzz")
  ;; => nil
  ())

(defn ag-converter-seq
  "Sequence from start type until it runs off the end."
  [ag-data start]
  (reify clojure.lang.ISeq
    (first [this]
      start)
    (next [this]
      (when-let [thence (next-category ag-data start)]
        (ag-converter-seq ag-data thence)))
    (more [this]
      (or (next this) ()))
    (seq [this]
      this)))

(comment
  #_(take 30 (ag-converter-seq (ag-parse sample-5) "seed"))
  (take 3 (ag-converter-seq (ag-parse sample-5) "seed"))
  ;; => ("seed" "soil" "fertilizer")
  (take 3 (ag-converter-seq (ag-parse sample-5) "fertilizer"))
  ;; => ("fertilizer" "water" "light")
  (take 3 (ag-converter-seq (ag-parse sample-5) "zzz"))
  ;; => ("zzz") ;; eh, close enough for now
  ())

(defn ag-converter
  "ag-data is the result of parsing mapper source via (ag-parse).
  from is the starting type (e.g. \"seed\", )"
  ([ag-data]
   (ag-converter ag-data "seed" "location"))
  ([ag-data from to]
   (let [category-conversions (partition 2 1 (ag-converter-seq ag-data from))
         conversion-ranges (map (fn [conversion]
                                  (get-in (concat [:mappers] conversion) ag-data))
                                category-conversions)
         conversion-fns (map converter conversion-ranges)]
     #_(reduce (fn [conv f]
               (comp f conv))
             conversion-fns)
     ;; category-conversions
     conversion-ranges
     )))

#_((comp inc #(* % %)) 4)
;; => 17

(get-in (ag-parse sample-5) (concat [:mappers] ["seed" "soil"]))

(ag-converter (ag-parse sample-5))
;; => "location"
((ag-converter (ag-parse sample-5)) 79)


#_(puzzle/in 2023 5)

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

;; day 4 part 2
;; plan of attack: roll back-to-front in number of winning cards.
;; becomes a dynamic programming solution. Numbers will get big fast.

(defn card-number [card-line]
  (-> card-line
      (string/split #":")
      first
      (string/split #"\s+")
      second
      edn/read-string))

(comment
  (map card-number sample-4)
  ;; => (1 2 3 4 5 6)
  ())

(comment
  (mapv winning-numbers-you-have sample-4)
  [4 2 2 1 0 0])

(defn cards-won
  ([card-lines]
   (let [winning-number-vec (mapv winning-numbers-you-have card-lines)
         idx (dec (count winning-number-vec))
         acc {}]
     (cards-won winning-number-vec idx acc)))
  ([winning-number-vec idx acc]
   (def *dbg* {:winning-number-vec winning-number-vec :idx idx :acc acc})
   (if (< idx 0)
     acc
     (let [num-copies-won (get winning-number-vec idx)
           idx-copies-won (range (inc idx) (+ (inc idx) num-copies-won))
           total-cards-won (reduce +
                                   1
                                   (map (fn [i]
                                          (get acc i 0))
                                        idx-copies-won))]
       (recur winning-number-vec (dec idx) (assoc acc idx total-cards-won))))))

(comment
  (cards-won sample-4)
  {5 1,
   4 1,
   3 2,
   2 4,
   1 7,
   0 15}

  (reduce + (map second (seq (cards-won sample-4))))
  ;; => 30

  (reduce + (map second (seq (cards-won (puzzle/in-lines 2023 4)))))
  ;; => 13261850
  ())


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

