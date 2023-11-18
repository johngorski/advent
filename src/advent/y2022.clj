(ns advent.y2022
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as sets]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [instaparse.core :as insta]))

(defn in [day]
  (map edn/read-string
       (string/split-lines (slurp (io/resource (str "2022/" day ".txt"))))))

(defn in-lines [day]
  (string/split-lines (slurp (io/resource (str "2022/" day ".txt")))))

;; Day 8

(def sample-8
  (string/split
   "30373
25512
65332
33549
35390
" #"\n"))

(defn forest-from [input]
  (mapv (fn [row] (mapv edn/read-string (string/split row #""))) input))

(comment
  (forest-from sample-8)
  (forest-from (in-lines 8)))

;; Let's traverse the forest from N, S, E, and W, and for each row, count
;; the number of visible trees.

;; Okay fine, sometimes it's easier to write it out imperatively first to
;; keep one's head straight.
;;
;; max-height := -1
;; visible-trees := #{}
;; for (tree of line) {
;;   h := (tree-height tree)
;;   if (h > max-height) {
;;     max := h
;;     (conj visible-trees tree)
;;     if (max-height = 9) break;
;;   }
;; }
;; return (count visible-trees)
;;
;; So we can do a loop/recur and get an answer, even though there's probably
;; some combination of seq functions which would give us a more elegant
;; answer. Probably some transducer-y thing. Worth looking up.
;;
;; Upon further consideration, this may also simply be (reduce) vs. a seq of
;; trees with their heights. Perhaps the tricky part would be terminating the
;; reduction early once we see the first 9.

(defn tree-line [forest start-tree [delta-row delta-col]]
  (let [max-row (dec (count forest))
        max-col (dec (count (first forest)))
        in-forest? (fn [[row col]] (and (<= 0 row max-row) (<= 0 col max-col)))]
    (take-while
     in-forest?
     (iterate
      (fn [[r c]]
        [(+ r delta-row)
         (+ c delta-col)])
      start-tree))
    ))

(comment
  (tree-line sample-8 [0 0] [0 1])
  (tree-line sample-8 [0 0] [1 0])
  (tree-line sample-8 [1 1] [1 0])
  )

(defn tree-height [forest tree]
  (get-in forest tree))

(comment
  (tree-height forest [1 1]))

(defn visible-trees [forest line]
  (loop [max-height -1
         visible #{}
         [tree & remaining] line]
    (if (or (nil? tree) (<= 9 max-height))
      visible
      (let [h (tree-height forest tree)]
        (recur
         (max h max-height)
         (if (< max-height h)
           (conj visible tree)
           visible)
         remaining
         )))))

(comment
  (let [forest (forest-from sample-8)]
    (visible-trees forest (tree-line forest [0 0] [0 1]))))

;; Good start. Last bit for part 1:
;; - take all the perimeter trees and face them inwards
;; - get the sets of all visible trees
;; - counts the union of all those sets.

(comment
  (in-lines 8))

;; Day 7

(def sample-7
  ["$ cd /"
   "$ ls"
   "dir a"
   "14848514 b.txt"
   "8504156 c.dat"
   "dir d"
   "$ cd a"
   "$ ls"
   "dir e"
   "29116 f"
   "2557 g"
   "62596 h.lst"
   "$ cd e"
   "$ ls"
   "584 i"
   "$ cd .."
   "$ cd .."
   "$ cd d"
   "$ ls"
   "4060174 j"
   "8033020 d.log"
   "5626152 d.ext"
   "7214296 k"])

(def parse-log-line
  (insta/parser
   "line = cd | ls | directory | file
    ls = '$ ls'
    cd = '$ cd ' dir
    dir = '/' | '..' | #'\\w+'
    file = size #'\\s+' name
    directory = 'dir ' dir
    name = #'[A-Za-z0-9.]+'
    size = #'\\d+'"))

(comment
  (parse-log-line "$ ls")
  ;; => [:line [:ls "$ ls"]]
  (parse-log-line "$ cd xyz")
  ;; => [:line [:cd "$ cd " [:dir "xyz"]]]
  (parse-log-line "dir d")
  ;; => [:line [:directory "dir " [:dir "d"]]]

  (map parse-log-line sample-7)

  [:line [:cd "$ cd " [:dir "/"]]]
  [:line [:ls "$ ls"]]
  [:line [:directory "dir " [:dir "a"]]]
  [:line [:file [:size "14848514"] " " [:name "b.txt"]]]
  [:line [:file [:size "8504156"] " " [:name "c.dat"]]]
  [:line [:directory "dir " [:dir "d"]]]
  [:line [:cd "$ cd " [:dir "a"]]]
  [:line [:ls "$ ls"]]
  [:line [:directory "dir " [:dir "e"]]]
  [:line [:file [:size "29116"] " " [:name "f"]]]
  [:line [:file [:size "2557"] " " [:name "g"]]]
  [:line [:file [:size "62596"] " " [:name "h.lst"]]]
  [:line [:cd "$ cd " [:dir "e"]]]
  [:line [:ls "$ ls"]]
  [:line [:file [:size "584"] " " [:name "i"]]]
  [:line [:cd "$ cd " [:dir ".."]]]
  [:line [:cd "$ cd " [:dir ".."]]]
  [:line [:cd "$ cd " [:dir "d"]]]
  [:line [:ls "$ ls"]]
  [:line [:file [:size "4060174"] " " [:name "j"]]]
  [:line [:file [:size "8033020"] " " [:name "d.log"]]]
  [:line [:file [:size "5626152"] " " [:name "d.ext"]]]
  [:line [:file [:size "7214296"] " " [:name "k"]]])

(defmulti info<-line (fn [[_ [line-type]]] line-type))

(defmethod info<-line :ls [_] [:ls])
(defmethod info<-line :cd [[_ [_ _ [_ dir]]]] [:cd dir])
(defmethod info<-line :directory [[_ [_ _ dir]]] dir)
(defmethod info<-line :file [[_ [_ [_ size] _ [_ name]]]] [:file name (edn/read-string size)])

(comment
  (info<-line [:line [:ls "$ ls"]])
  ;; => [:ls]
  (info<-line [:line [:cd "$ cd " [:dir ".."]]])
  ;; => [:cd ".."]
  (info<-line [:line [:directory "dir " [:dir "a"]]])
  ;; => [:dir "a"]
  (info<-line [:line [:file [:size "4060174"] " " [:name "j"]]])
  ;; => [:file "j" 4060174]

  {"a" {"e" {"i" 584}
        "f" 29116
        "g" 2557
        "h.lst" 62596}
   "b.txt" 14848514
   "c.dat" 8504156
   "d" {"j" 4060174
        "d.log" 8033020
        "d.ext" 5626152
        "k" 7214296}}

  )

;; Y'know, maybe forget the zipper and just keep track of the file structure
;; and current directory?

(defn update-fs [[cmd & args]]
  (case cmd
    :ls
    identity

    :cd
    (let [dir (first args)]
      (fn [shell]
        (update shell
                :working-directory
                (case dir
                  ".."
                  butlast

                  "/"
                  (fn [_] ())

                  ;; else
                  #(concat % [dir]))
                )))

    :dir
    (let [dir (first args)]
      (fn [{:keys [working-directory] :as shell}]
        (update-in shell
                   (concat [:files] working-directory [dir])
                   #(or % {})
                   )))

    :file
    (let [[file size] args]
      (fn [{:keys [working-directory] :as shell}]
        (assoc-in shell
                  (concat [:files] working-directory [file])
                  size)))))


(defn infer-fs [infos]
  (let [updates (map update-fs infos)]
    (:files (reduce #(%2 %1) {:files {} :working-directory ()} updates))))


(defn files-from-input [input]
  (infer-fs (map (comp info<-line parse-log-line) input)))

(comment
  (files-from-input sample-7)
  {"a" {"e" {"i" 584},
        "f" 29116,
        "g" 2557,
        "h.lst" 62596},
   "b.txt" 14848514,
   "c.dat" 8504156,
   "d" {"j" 4060174,
        "d.log" 8033020,
        "d.ext" 5626152,
        "k" 7214296}})

(defn total-size [files path]
  (let [dir (get-in files path)]
    (cond
      (number? dir)
      dir

      (empty? dir)
      0

      :else ;; map
      (reduce + (map (fn [key] (total-size files (concat path [key]))) (keys dir)))
      ))
  )

(comment
  (total-size (files-from-input sample-7) ["a" "e"])
  (total-size (files-from-input sample-7) ["a"])
  (total-size (files-from-input sample-7) ["d"])
  (total-size (files-from-input sample-7) [])
  )

(defn all-directories [files from]
  (let [current-directory (get-in files from)]
    (when (map? current-directory)
      (concat [from] (mapcat (fn [subdir] (all-directories files (concat from [subdir]))) (keys current-directory)))
      )))

(comment
  (all-directories (files-from-input sample-7) ())
  ;; => (() ("a") ("a" "e") ("d"))
  (comment
    (let [files (files-from-input sample-7)]
      (map (fn [dir] [dir (total-size files dir)]) (all-directories files ()))))
  ;; => ([() 48381165] [("a") 94853] [("a" "e") 584] [("d") 24933642])
  )

(defn file-sizes [input]
  (let [files (files-from-input input)]
    (map (fn [dir] (total-size files dir)) (all-directories files ()))
    ))

(defn total-size-sum [input]
  (reduce + (let [files (files-from-input input)
                  sizes (map (fn [dir] (total-size files dir)) (all-directories files ()))]
              (filter #(<= % 100000) sizes))))

(comment
  (total-size-sum sample-7)
  ;; => 95437
  (total-size-sum (in-lines 7))
  ;; => 1490523
  )

;; Part 2
(defn smallest-needed [input]
  (let [total-disk 70000000
        space-needed 30000000
        files (files-from-input input)
        used (total-size files ())
        current-unused (- total-disk used)
        required (- space-needed current-unused)]
    (apply min (filter #(<= required %) (file-sizes input)))
    ))

(comment
  (smallest-needed sample-7)
  ;; => 24933642

  (smallest-needed (in-lines 7))
  ;; => 12390492
  )

;; Day 6

(defn packet-start
  ([input] (packet-start 4 (take 4 input) (drop 4 input)))
  ([index window remainder]
   (if (= 4 (count (set window)))
     index
     (let [[head & tail] remainder]
       (recur (inc index) (concat (drop 1 window) [head]) tail)))))

;; (packet-start "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
;; => 7

;; (packet-start (first (in-lines 6)))
;; => 1896

(defn message-start
  ([input] (message-start 14 (take 14 input) (drop 14 input)))
  ([index window remainder]
   (if (= 14 (count (set window)))
     index
     (let [[head & tail] remainder]
       (recur (inc index) (concat (drop 1 window) [head]) tail)))))

(comment
  (map message-start [
                      "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
                      "bvwbjplbgvbhsrlpgdmjqwftvncz"
                      "nppdvjthqldpwncqszvftbrmjlhg"
                      "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
                      "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
                      ]))
;; => (19 23 23 29 26)

;; (message-start (first (in-lines 6)))
;; => 3452

;; Day 5

(defn stack [s]
  (reduce conj () (reverse (map str s))))

(comment
  {"1" (stack "QGPRLCTF")
   "2" (stack "JSFRWHQN")
   "3" (stack "QMPWHBF")
   "4" (stack "FDTSV")
   "5" (stack "ZFVWDLQ")
   "6" (stack "SLCZ")
   "7" (stack "FDVMBZ")
   "8" (stack "BJT")
   "9" (stack "HPSLGBNQ")}

  (first (drop 10 (in-lines 5))))
;; => "move 1 from 8 to 1"

(defn stack-step [s]
  (let [[_ rep-str from to]
        (re-matches #"move (\d+) from (\d+) to (\d+)" s)]
    (repeat (edn/read-string rep-str) (fn [stacks]
                                        (let [crate (first (stacks from))]
                                          (-> stacks
                                              (update from rest)
                                              (update to conj crate)))))))

(comment
  (let [stacks {"1" (stack "NZ")
                "2" (stack "DCM")
                "3" (stack "P")}
        steps ["move 1 from 2 to 1"]]
    (reduce #(%2 %1) stacks (mapcat stack-step steps)))
  ;; => {"1" ("D" "N" "Z"), "2" ("C" "M"), "3" ("P")}

  (let [stacks {"1" (stack "NZ")
                "2" (stack "DCM")
                "3" (stack "P")}
        steps ["move 1 from 2 to 1"
               "move 3 from 1 to 3"]]
    (reduce #(%2 %1) stacks (mapcat stack-step steps)))
  ;; => {"1" (), "2" ("C" "M"), "3" ("Z" "N" "D" "P")}

  (let [stacks {"1" (stack "NZ")
                "2" (stack "DCM")
                "3" (stack "P")}
        steps ["move 1 from 2 to 1"
               "move 3 from 1 to 3"
               "move 2 from 2 to 1"]]
    (reduce #(%2 %1) stacks (mapcat stack-step steps)))
  ;; => {"1" ("M" "C"), "2" (), "3" ("Z" "N" "D" "P")}

  (let [stacks {"1" (stack "NZ")
                "2" (stack "DCM")
                "3" (stack "P")}
        steps ["move 1 from 2 to 1"
               "move 3 from 1 to 3"
               "move 2 from 2 to 1"
               "move 1 from 1 to 2"]]
    (reduce #(%2 %1) stacks (mapcat stack-step steps))))
;; => {"1" ("C"), "2" ("M"), "3" ("Z" "N" "D" "P")}

(defn move-crates [stacks procedure]
  (reduce #(%2 %1) stacks (mapcat stack-step procedure)))

(comment
  (move-crates
   {"1" (stack "NZ")
    "2" (stack "DCM")
    "3" (stack "P")}
   ["move 1 from 2 to 1"
    "move 3 from 1 to 3"
    "move 2 from 2 to 1"
    "move 1 from 1 to 2"])
  ;; => {"1" ("C"), "2" ("M"), "3" ("Z" "N" "D" "P")}

  (let [final-crates
        (move-crates
         {"1" (stack "QGPRLCTF")
          "2" (stack "JSFRWHQN")
          "3" (stack "QMPWHBF")
          "4" (stack "FDTSV")
          "5" (stack "ZFVWDLQ")
          "6" (stack "SLCZ")
          "7" (stack "FDVMBZ")
          "8" (stack "BJT")
          "9" (stack "HPSLGBNQ")}
         (drop 10 (in-lines 5)))]
    (apply str (map (comp first final-crates str inc) (range 9)))))
;; => "VGBBJCRMN"

(defn stack-9001 [s]
  (into [] (map str) s))

;; (stack-9001 "abc")
;; => ["a" "b" "c"]

(defn stack-step-9001 [s]
  (let [[_ rep-str from to]
        (re-matches #"move (\d+) from (\d+) to (\d+)" s)
        crates (edn/read-string rep-str)]
    (fn [stacks]
      (let [substack (take crates (stacks from))]
        (-> stacks
            (update from #(drop crates %))
            (update to #(concat substack %)))))))

(defn move-crates-9001 [stacks procedure]
  (reduce #(%2 %1) stacks (map stack-step-9001 procedure)))

(comment
  (let [final-crates
        (move-crates-9001
         {"1" (stack "QGPRLCTF")
          "2" (stack "JSFRWHQN")
          "3" (stack "QMPWHBF")
          "4" (stack "FDTSV")
          "5" (stack "ZFVWDLQ")
          "6" (stack "SLCZ")
          "7" (stack "FDVMBZ")
          "8" (stack "BJT")
          "9" (stack "HPSLGBNQ")}
         (drop 10 (in-lines 5)))]
    (apply str (map (comp first final-crates str inc) (range 9)))))
;; => "LBBVJBRMH"

;; Day 4

(defn assignment [s]
  (map edn/read-string (string/split s #"-")))

;; (assignment "2-4")
;; => (2 4)

(defn assignment-pair [s]
  (map assignment (string/split s #",")))

;; (assignment-pair "2-4,6-8")
;; => ((2 4) (6 8))

(defn fully-overlaps? [left right]
  (let [[l-bottom l-top] left
        [r-bottom r-top] right]
    (cond
      (= l-bottom r-bottom) true
      (< r-bottom l-bottom) (recur right left)
      :else (<= r-top l-top))))

;; (fully-overlaps? [0 5] [1 3])
;; => true
;; (fully-overlaps? [0 3] [1 5])
;; => false

;; (count (filter #(apply fully-overlaps? %) (map assignment-pair (in-lines 4))))
;; => 571

(defn overlaps? [left right]
  (let [[l-bottom l-top] left
        [r-bottom r-top] right]
    (cond
      (= l-bottom r-bottom) true
      (< r-bottom l-bottom) (recur right left)
      :else (<= r-bottom l-top))))

;; (overlaps? [5 7] [7 9])
;; => true
;; (overlaps? [5 7] [8 9])
;; => false

;; (count (filter #(apply overlaps? %) (map assignment-pair (in-lines 4))))
;; => 917

;; Day 3

;; Find the item type that appears in both compartments of each rucksack.
;; What is the sum of the priorities of those item types?

(defn both-compartments-item [rucksack]
  (let [[left right] (partition (/ (count rucksack) 2) rucksack)]
    (first (sets/intersection (set left) (set right)))))
;; => #'advent.y2022/both-compartments-item

;; (both-compartments-item "vJrwpWtwJgWrhcsFMMfFFhFp")
;; => \p

(defn item-priority [item]
  (let [A (int \A)
        Z (int \Z)
        a (int \a)
        z (int \z)
        i (int item)]
    (cond
      (<= a i z)
      (inc (- (int item) (int \a)))

      (<= A i Z)
      (+ 27 (- (int item) (int \A)))
      )))

(comment
  (item-priority \a)
  ;; => 1
  (item-priority \z)
  ;; => 26
  (item-priority \A)
  ;; => 27

  (reduce + (map (comp item-priority both-compartments-item) (in-lines 3))))
;; => 7793

(defn badge-item [rucksacks]
  (first (apply sets/intersection (map set rucksacks))))

(comment
  (badge-item
   ["vJrwpWtwJgWrhcsFMMfFFhFp"
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
    "PmmdzqPrVvPwwTWBwg"])
  ;; => \r

  (reduce + (map (comp item-priority badge-item) (partition 3 (in-lines 3)))))
;; => 2499

;; Day 2

(defn rps-result [them you]
  (get-in
   {:Rock {:Rock :tie
           :Paper :win
           :Scissors :loss}
    :Paper {:Rock :loss
            :Paper :tie
            :Scissors :win}
    :Scissors {:Rock :win
               :Paper :loss
               :Scissors :tie}}
   [them you]))

(comment
  (rps-result :Rock :Scissors)
  ;; => :loss
  (rps-result :Scissors :Rock))
;; => :win

(defn rps-score [them you]
  (let [result (rps-result them you)]
    (+
     ({:loss 0
       :tie 3
       :win 6}
      result)
     ({:Rock 1
       :Paper 2
       :Scissors 3}
      you))))

(comment
  (rps-score :Rock :Paper)
  ;; => 8
  (rps-score :Paper :Rock)
  ;; => 1
  (rps-score :Scissors :Scissors))
;; => 6

(defn rps-decode [key moves]
  (let [[abc xyz] moves]
    (rps-score (key abc) (key xyz))))

(defn rps-strategy-score [key rounds]
  (reduce + (map #(rps-decode key %) rounds)))

(comment
  (let [key {"A" :Rock
             "B" :Paper
             "C" :Scissors
             "X" :Rock
             "Y" :Paper
             "Z" :Scissors}]
    (map #(rps-decode key %) (map #(string/split % #"\s+") (in-lines 2))))

  (rps-strategy-score
   {"A" :Rock
    "B" :Paper
    "C" :Scissors
    "X" :Rock
    "Y" :Paper
    "Z" :Scissors}
   [["A" "Y"]
    ["B" "X"]
    ["C" "Z"]]))

(comment
  (rps-strategy-score
   {"A" :Rock
    "B" :Paper
    "C" :Scissors
    "X" :Rock
    "Y" :Paper
    "Z" :Scissors}
   (map #(string/split % #"\s+") (in-lines 2)))
  ;; => 11603

  (take 3 (map #(string/split % #"\s+") (in-lines 2)))
  ;; => (["B" "Y"] ["A" "Z"] ["A" "Z"])
  (last (map #(string/split % #"\s+") (in-lines 2)))
  ;; => ["A" "Z"]
  (first (map #(string/split % #"\s+") (in-lines 2))))
;; => ["B" "Y"]

(defn rps-2-score [abc xyz]
  (get-in {
           ;; vs Rock
           "A" {"X" (+ 0 3)
                "Y" (+ 3 1)
                "Z" (+ 6 2)}
           ;; vs Paper
           "B" {"X" (+ 0 1)
                "Y" (+ 3 2)
                "Z" (+ 6 3)}
           ;; vs Scissors
           "C" {"X" (+ 0 2)
                "Y" (+ 3 3)
                "Z" (+ 6 1)}
           } [abc xyz]))

(comment
  (reduce + (map #(apply rps-2-score %) (map #(string/split % #"\s+") (in-lines 2)))))
;; => 12725

;; Day 1
(comment

  (apply max (filter some? (map #(apply + %) (partition-by some? (in 1)))))
  ;; => 71502

  (apply + (take 3 (reverse (sort (filter some? (map #(apply + %) (partition-by some? (in 1)))))))))
;; => 208191
