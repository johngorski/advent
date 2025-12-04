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

