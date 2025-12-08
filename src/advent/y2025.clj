(ns advent.y2025
  (:require
   [advent.grid :as grids]
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

(def sample-3
 "987654321111111
811111111111119
234234234234278
818181911112111")

(defn parse-bank [line]
  (map (comp edn/read-string str) (seq line)))

(comment
  (parse-bank "987654321111111")

  (apply max (butlast (parse-bank "987654321111111")))

  (drop 1 (drop-while (fn [d] (not= 9 d)) (parse-bank "987654321111111"))))


(defn bank-joltage [bank]
  (let [high-digit (apply max (butlast bank))
        low-digit (apply max (rest (drop-while (fn [d]
                                                   (not= high-digit d))
                                                 bank)))]
    (+ (* 10 high-digit) low-digit)))

(defn day-3-1 [lines]
  (reduce +
          (map (comp bank-joltage parse-bank) lines)))

(comment
  (map (comp bank-joltage parse-bank) (string/split-lines sample-3))
  ;; => (98 89 78 92)

  (day-3-1 (string/split-lines sample-3))
  ;; => 357

  (day-3-1 (in-lines 3))
  ;; => 17113
  ())


(defn joltage-digit
  "10^ith digit of the bank given the passed fragment"
  [i bank-fragment]
  (apply max (drop-last i bank-fragment)))

(defn fragment-after
  "Remaining bank-fragment after we traverse to the fragment digit n"
  [n bank-fragment]
  (rest (drop-while (fn [d]
                      (not= n d))
                    bank-fragment)))

(defn n-joltage
  ([n bank] (n-joltage 0 n bank))
  ([acc n bank]
   (let [j-d (joltage-digit n bank)
         acc' (+ acc j-d)]
     (if (zero? n)
       acc'
       (recur (* 10 acc')
              (dec n)
              (fragment-after j-d bank))))))

(comment
  (n-joltage 12 (map (comp edn/read-string str) (seq "234234234234278")))
  ;;    0000000001111
  ;;    1234567890123
  ;; => 4234234234278
  ;; => 423423423435
  ;; 434234234278 ;; not quite....
  (n-joltage 11 (map (comp edn/read-string str) (seq "234234234234278")))
  ;; => 434234234278
  ;;  : 434234234278
  ())

(defn day-3-2 [lines]
  (reduce +
          (map (comp (fn [bank] (n-joltage 11 bank)) parse-bank) lines)))

(comment
  (day-3-2 (string/split-lines sample-3))
  ;; => 3121910778619
  ;;  : 3121910778619

  (day-3-2 (in-lines 3))
  ;; => 169709990062889
  ())

;; Day 4

(def sample-4
  "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

(def sample-4-grid
  (grids/grid (string/split-lines sample-4)))


(grids/cells-at sample-4-grid
          (grids/adjacent-neighbor-locs sample-4-grid [0 1]))


(def tp-val "@")

(defn tp-locs [grid]
  (grids/val-grid-locations tp-val grid))

(defn forklift-accessible? [grid tp-loc]
  (let [adjacent-cells (grids/cells-at grid (grids/adjacent-neighbor-locs grid tp-loc))
        adjacent-tp-locs (filter (fn [cell]
                                   (= tp-val cell))
                                 adjacent-cells)
        adjacent-tp (count adjacent-tp-locs)]
    (< adjacent-tp 4)))


(defn forklift-accessible-tp [grid]
  (filter (fn [tp-loc]
            (forklift-accessible? grid tp-loc))
          (grids/val-grid-locations tp-val grid)))

(comment
  (count
   (forklift-accessible-tp sample-4-grid))
  ;; => 13
  ())

;; day 4 part 1
(comment
  (count
   (forklift-accessible-tp (grids/grid (in-lines 4))))
  ;; => 1372
  ())

(defn remove-tp [grid locs]
  (reduce (fn [g loc]
            (assoc-in g loc "."))
          grid
          locs))

(defn inaccessible-tp-locs [grid]
  (let [remaining-tp-locs (set (tp-locs grid))
        accessible-tp-locs  (filter (fn [loc] (forklift-accessible? grid loc)) remaining-tp-locs)]
    (if (empty? accessible-tp-locs)
      remaining-tp-locs
      (recur (remove-tp grid accessible-tp-locs)))))

;; (inaccessible-tp-locs sample-4-grid)

(defn tp-count [grid]
  (count (tp-locs grid)))

(defn day-4-part-2 [grid]
  (- (tp-count grid) (count (inaccessible-tp-locs grid))))

(comment
  (day-4-part-2 sample-4-grid)
  ;; => 43
  (day-4-part-2 (grids/grid (in-lines 4)))
  ;; => 7922
  ())

;; Day 5


(def sample-5-lines
  (string/split-lines
   "3-5
10-14
16-20
12-18

1
5
8
11
17
32"))

(defn parse-range [line]
  (let [[lo-s hi-s] (string/split line #"-")]
    [(edn/read-string lo-s) (edn/read-string hi-s)]))

(comment
  (parse-range "3-5")
  ;; => [3 5]
  ())

(defn parse-id [line]
  (edn/read-string line))

(defn parse-lines-5 [lines]
  (let [range-lines (take-while not-empty lines)
        id-lines (rest (drop-while not-empty lines))]
    {:ranges (map parse-range range-lines)
     :ids (map parse-id id-lines)}))

(comment
  (parse-lines-5 sample-5-lines)
  ;; => {:ranges ([3 5] [10 14] [16 20] [12 18]), :ids (1 5 8 11 17 32)}
  ())

(defn inclusive-range-checker [[lo hi]]
  (fn [n]
    (<= lo n hi)))

(comment
  ((inclusive-range-checker [3 5]) 4)
  ;; => true
  ((inclusive-range-checker [3 5]) 2)
  ;; => false
  ((inclusive-range-checker [3 5]) 6)
  ;; => false
  ())

(defn fresh-checker [ranges]
  (let [any? (complement not-any?)
        bounds-checkers (map inclusive-range-checker ranges)]
    (fn [id]
      (any? (fn [in-bounds?]
              (in-bounds? id))
            bounds-checkers))))

(defn day-5a [lines]
  (let [{:keys [ranges ids]} (parse-lines-5 lines)
        fresh? (fresh-checker ranges)]
    (count (filter fresh? ids))))

(comment
  (day-5a sample-5-lines)
  ;; => 3

  (day-5a (in-lines 5))
  ;; => 828
  ())


(defn overlapping-ranges
  "The old-ranges which overlap with the new range."
  [old-ranges new-range]
  (let [[lo-new hi-new] new-range
        in-new-range? (inclusive-range-checker new-range)
        overlap? (fn [old-range]
                   (let [[lo-old hi-old] old-range]
                     (or (in-new-range? lo-old)
                         (in-new-range? hi-old)
                         (let [in-old-range? (inclusive-range-checker old-range)]
                           (or (in-old-range? lo-new)
                               (in-old-range? hi-new))))))]
    (into #{new-range}
          (filter overlap?)
          old-ranges)))


(defn combined-range
  "Combines the given ranges into one range. Assumes they overlap."
  [ranges]
  [(apply min (map first ranges))
   (apply max (map second ranges))])


(defn absorb-range
  "Absorbs the new range into the set of disjoint old ranges."
  [old-ranges new-range]
  (let [overlapping (overlapping-ranges old-ranges new-range)]
    (-> old-ranges
        (sets/difference overlapping)
        (conj (combined-range overlapping)))))


(defn combined-ranges
  "Set of disjoint inclusive ranges"
  [ranges]
  (reduce absorb-range #{} ranges))


(defn inclusive-range-size [[lo hi]]
  (inc (- hi lo)))

(defn day-5b [lines]
  (->> lines
       parse-lines-5
       :ranges
       combined-ranges
       (map inclusive-range-size)
       (reduce +)))

(comment
  (day-5b sample-5-lines)
  ;; => 14

  (day-5b (in-lines 5))
  ;; => 352681648086146
  ())


;; Day 6

(def sample-6-lines
  (string/split-lines
   "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  "))

(comment
  (map (fn [line] (take 20 line)) (in-lines 6))
  (count (in-lines 6)))

(defn row-numbers [line]
  (map edn/read-string (string/split (string/trim line) #"\s+")))

(comment
  (row-numbers (first sample-6-lines))
  ;; => (123 328 51 64)
  (row-numbers (last sample-6-lines))
  ;; => (* + * +)
  ())

(defn transpose [rows]
  (apply map vector rows))


(defn number-rows [lines]
  (map row-numbers (butlast lines)))

(comment
  (transpose (number-rows sample-6-lines))
  ;; => ([123 45 6] [328 64 98] [51 387 215] [64 23 314])
  ())


(defn operands [lines]
  (transpose (number-rows lines)))

(comment
  (operands sample-6-lines)
  ;; => ([123 45 6] [328 64 98] [51 387 215] [64 23 314])
  ())

(defn operators [lines]
  (map {'+ + '* *} (row-numbers (last lines))))

(comment
  (operators sample-6-lines)
  ;; => (* + * +)
  ())

(defn worksheet-solutions [ops args]
  (map apply ops args))

(comment
  (worksheet-solutions (operators sample-6-lines) (operands sample-6-lines))
  ;; => (33210 490 4243455 401)
  ())

(defn day-6a [lines]
  (let [[ops args] ((juxt operators operands) lines)]
    (reduce + (worksheet-solutions ops args))))

(comment
  (day-6a sample-6-lines)
  ;; => 4277556
  ()

  (day-6a (in-lines 6))
  ;; => 5227286044585
  ()

  (remove #(= % '(()))
          (partition-by #(= % ())
                        (map (fn [num-chars]
                               (remove (fn [n-char]
                                         (= \space n-char))
                                       num-chars))
                             (reverse (transpose sample-6-lines))))))
(comment
  (((\4)
    (\4 \3 \1)
    (\6 \2 \3 \+))
   ((\1 \7 \5)
    (\5 \8 \1)
    (\3 \2 \*))
   ((\8)
    (\2 \4 \8)
    (\3 \6 \9 \+))
   ((\3 \5 \6)
    (\2 \4)
    (\1 \*))))

(comment
  ((\4)
   (\4 \3 \1)
   (\6 \2 \3 \+)
   ()
   (\1 \7 \5)
   (\5 \8 \1)
   (\3 \2 \*)
   ()
   (\8)
   (\2 \4 \8)
   (\3 \6 \9 \+)
   ()
   (\3 \5 \6)
   (\2 \4)
   (\1 \*)))

(defn split-problems [lines]
  (->> (reverse (transpose lines))
       (map (fn [num-chars]
              (remove (fn [n-char]
                        (= \space n-char))
                      num-chars)))
       (partition-by #(= % ()))
       (remove #(= % '(())))))

(comment
  (split-problems sample-6-lines)
  (((\4)
    (\4 \3 \1)
    (\6 \2 \3 \+))
   ((\1 \7 \5)
    (\5 \8 \1)
    (\3 \2 \*))
   ((\8)
    (\2 \4 \8)
    (\3 \6 \9 \+))
   ((\3 \5 \6)
    (\2 \4)
    (\1 \*))))

(defn problem-operator [problem]
  (some {\+ + \* *} (apply concat problem)))

#_(problem-operator
 '((\4)
   (\4 \3 \1)
   (\6 \2 \3 \+)))
;; => #object[clojure.core$_PLUS_ 0x65c868c9 "clojure.core$_PLUS_@65c868c9"]

(defn problem-operands [problem]
  (->> problem
       (map (fn [col]
              (->> col
                   (filter (set "1234567890"))
                   (apply str)
                   edn/read-string)))))

#_(problem-operands
 '((\4)
   (\4 \3 \1)
   (\6 \2 \3 \+)))
;; => (4 431 623)
;; (filter (set "1234567890") "h4x0r")

(defn day-6b [lines]
  (let [problems (split-problems lines)
        [ops args] ((juxt (partial map problem-operator) (partial map problem-operands)) problems)]
    (reduce + (worksheet-solutions ops args))))

(comment
  (day-6b sample-6-lines)
  ;; => 3263827

  (day-6b (in-lines 6))
  ;; => 10227753257799
  ())


;; Day 7

(def sample-7-lines
  (string/split-lines
   ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
..............."))


(comment
  (string/index-of "banana" \n)
  ;; => 2
  (string/index-of "banana" \n 3)
  ;; => 4
  (string/index-of "banana" \n 5)
  ;; => nil

  (string/index-of "banana" "a")
  ;; => 1
  (string/index-of "banana" "a" (inc 1))
  ;; => 3
  (string/index-of "banana" "a" (inc 3))
  ;; => 5
  (string/index-of "banana" "a" (inc 5))
  ;; => nil
  (string/index-of "banana" "b" 0)
  ;; => 0
  ())


(defn splitter-indices
  ([line] (splitter-indices #{} -1 line))
  ([found last-found-at line]
   (if-let [next-found-at (string/index-of line "^" (inc last-found-at))]
     (recur (conj found next-found-at) next-found-at line)
     found)))

(comment
  (splitter-indices "b^n^n^")
  ;; => #{1 3 5}
  ())

;; (sets/intersection #{2 3 5 7} (set (filter odd? (range 10))))


(defn beam-descends [{:keys [splits beam-indices]} splitter-indices]
  (let [hit-splitters (sets/intersection beam-indices splitter-indices)
        new-beams (set (mapcat (juxt dec inc) hit-splitters))]
    {:splits (+ splits (count hit-splitters))
     :beam-indices (-> beam-indices
                       (sets/difference hit-splitters)
                       (sets/union new-beams))}))

(defn start-beam [line]
  #{(string/index-of line "S")})

(comment
  (start-beam (first sample-7-lines))
  ;; => #{7}
  ())


(defn day-7a [lines]
  (:splits (reduce beam-descends
                   {:splits 0
                    :beam-indices (start-beam (first lines))}
                   (map splitter-indices (rest lines)))))

(comment
  (day-7a sample-7-lines)

  (day-7a (in-lines 7))
  ;; => 1660
  ()

  (select-keys {1 2 3 4 5 6} #{1 5})
  ;; => {1 2, 5 6}
  (select-keys {1 2 3 4 5 6} (seq #{1 5}))
  ;; => {1 2, 5 6}


  (apply dissoc {1 2 3 4 5 6} #{1 5})
  ;; => {3 4}

  (map identity {1 2 3 4 5 6})
  ;; => ([1 2] [3 4] [5 6])
  ())

(defn split-timelines [timelines]
  (reduce
   (fn [summed [k v]]
     (update summed
             k
             (fn [u]
               (+ (or u 0)
                  v))))
   {}
   (mapcat
    (fn [[k v]]
      [[(dec k) v]
       [(inc k) v]])
    timelines)))

(comment
  (split-timelines {1 2, 2 4, 3 2})
  ;; => {0 2, 2 4, 1 4, 3 4, 4 2}
  (split-timelines {1 1, 2 1, 3 1, 4 1, 5 1})
  ;; => {0 1, 2 2, 1 1, 3 2, 4 2, 5 1, 6 1}
  (split-timelines {1 1, 2 1, 3 1, 4 1})
  ;; => {0 1, 2 2, 1 1, 3 2, 4 1, 5 1}
  ())


(defn beam-timelines
  [timelines splitter-indices]
  (let [hit-splitters (sets/intersection (set (keys timelines)) splitter-indices)
        hit-timelines (select-keys timelines hit-splitters)]
    (def dbg* {:hit-splitters hit-splitters
               :hit-timelines hit-timelines
               :split-timelines split-timelines
               })
    (merge-with +
           (apply dissoc timelines hit-splitters)
           (split-timelines hit-timelines))))


(defn day-7b [lines]
    (let [timelines (reduce beam-timelines
                            (into {}
                                  (map (fn [k] [k 1]))
                                  (start-beam (first lines)))
                            (map splitter-indices (rest lines)))]
      (reduce + (vals timelines))
      #_timelines
      ))


(comment
  (day-7b sample-7-lines)
  ;; => 40
  (day-7b (in-lines 7))
  ;; => 305999729392659
  ())

;; Day 8

(def sample-8-lines
  (string/split-lines
   "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689"))


(defn parse-box-position [line]
  (mapv edn/read-string (string/split line #",")))

(defn displacement [from to]
  (map - to from))

(defn norm-squared [v]
  (reduce + (map (fn [x]
                   (* x x))
                 v)))

(defn distance-squared [from to]
  (norm-squared (displacement from to)))

(comment
  (distance-squared [1 2 3] [4 5 6])
  ;; => 27
  (parse-box-position "57,618,57")
  ;; => [57 618 57]
  ())

(defn box-distances [positions]
  (into {}
        (for [from positions
              to positions
              :when (not= from to)]
          [#{from to} (distance-squared from to)])))


;; (box-distances (map parse-box-position sample-8-lines))



(defn closest-boxes [distances]
  (map first
       (sort-by second distances)))


(defn create-new-circuit [{:keys [circuits in-circuit next-circuit-id] :as joined} from to]
  (assoc joined
         :circuits (assoc circuits next-circuit-id #{from to})
         :in-circuit (assoc in-circuit
                            from next-circuit-id
                            to next-circuit-id)
         :next-circuit-id (inc next-circuit-id)))

(defn add-to-circuit [{:keys [circuits in-circuit] :as joined} circuit box]
  (assoc joined
         :circuits (update circuits circuit conj box)
         :in-circuit (assoc in-circuit box circuit)))

(defn join-circuits [{:keys [circuits in-circuit] :as joined} circuit-a circuit-b]
  (let [[replaced-id combined-id] (sort [circuit-a circuit-b])]
    (assoc joined
           :circuits (-> circuits
                         (dissoc replaced-id)
                         (assoc combined-id (sets/union (circuits circuit-a) (circuits circuit-b))))
           :in-circuit (reduce (fn [in-circuit' moved-box]
                                 (assoc in-circuit' moved-box combined-id))
                               in-circuit
                               (circuits replaced-id)))))


(defn join-boxes
  "circuits - map from circuit ID to set of connected boxes
  in-circuit - map from box to circuit ID it's a part of
  next-circuit-id next unassigned circuit id for use in the above"
  [{:keys [circuits in-circuit next-circuit-id] :as joined} from to]
  (let [from-in (in-circuit from)
        to-in (in-circuit to)]
    ;; cases:
    (cond
      ;; - neither from nor to in a circuit -> create new circuit with next-circuit-id
      (and (nil? from-in) (nil? to-in))
      (create-new-circuit joined from to)

      ;; - from and to are in the same circuit -> return joined
      (= from-in to-in)
      joined

      ;; - one of from or to is in a circuit -> add the other that circuit
      (nil? from-in)
      (add-to-circuit joined to-in from)

      (nil? to-in)
      (add-to-circuit joined from-in to)

      ;; - from and to are in different circuits -> join the circuits
      :else ;; (not= from-in to-in)
      (join-circuits joined from-in to-in))))


(defn day-8a [n lines]
  (let [joined-circuits (reduce (fn [joined connection]
                                  (let [[from to] (seq connection)]
                                    (join-boxes joined from to)))
                                {:circuits {} :in-circuit {} :next-circuit-id 0}
                                (->> lines
                                     (map parse-box-position)
                                     box-distances
                                     closest-boxes
                                     (take n)))]
    joined-circuits))

(day-8a 10 sample-8-lines)

