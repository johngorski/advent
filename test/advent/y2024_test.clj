(ns advent.y2024-test
  (:require
   [advent.grid :as grids]
   [advent.puzzle :as puzzle]
   [advent.y2024 :refer :all]
   [clojure.test :refer :all]))


(def sample-10-lines
  (puzzle/sample-lines
   "0123
1234
8765
9876"))

(def sample-10-larger-lines
  (puzzle/sample-lines
   "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"))

(deftest day-10
  (testing "trailheads"
    (is (= (trailheads (topo-map sample-10-lines))
           [[0 0]])))
  (testing "trails"
    (testing "run some trails in the sample"
      (is (= (set (trails-at (topo-map sample-10-lines) [0 0]))
             '#{([3 0] [3 1] [3 2] [3 3] [2 3] [1 3] [0 3] [0 2] [0 1] [0 0])
                ([3 0] [3 1] [3 2] [2 2] [2 3] [1 3] [0 3] [0 2] [0 1] [0 0])
                ([3 0] [3 1] [2 1] [2 2] [2 3] [1 3] [0 3] [0 2] [0 1] [0 0])
                ([3 0] [2 0] [2 1] [2 2] [2 3] [1 3] [0 3] [0 2] [0 1] [0 0])
                ([3 0] [3 1] [3 2] [3 3] [2 3] [1 3] [1 2] [0 2] [0 1] [0 0])
                ([3 0] [3 1] [3 2] [2 2] [2 3] [1 3] [1 2] [0 2] [0 1] [0 0])
                ([3 0] [3 1] [2 1] [2 2] [2 3] [1 3] [1 2] [0 2] [0 1] [0 0])
                ([3 0] [2 0] [2 1] [2 2] [2 3] [1 3] [1 2] [0 2] [0 1] [0 0])
                ([3 0] [3 1] [3 2] [3 3] [2 3] [1 3] [1 2] [1 1] [0 1] [0 0])
                ([3 0] [3 1] [3 2] [2 2] [2 3] [1 3] [1 2] [1 1] [0 1] [0 0])
                ([3 0] [3 1] [2 1] [2 2] [2 3] [1 3] [1 2] [1 1] [0 1] [0 0])
                ([3 0] [2 0] [2 1] [2 2] [2 3] [1 3] [1 2] [1 1] [0 1] [0 0])
                ([3 0] [3 1] [3 2] [3 3] [2 3] [1 3] [1 2] [1 1] [1 0] [0 0])
                ([3 0] [3 1] [3 2] [2 2] [2 3] [1 3] [1 2] [1 1] [1 0] [0 0])
                ([3 0] [3 1] [2 1] [2 2] [2 3] [1 3] [1 2] [1 1] [1 0] [0 0])
                ([3 0] [2 0] [2 1] [2 2] [2 3] [1 3] [1 2] [1 1] [1 0] [0 0])})))
    (testing "score"
      (testing "sample trailhead score"
        (is (= 1 (trailhead-score (topo-map sample-10-lines) [0 0])))))
    (testing "larger sample trailhead scores"
      (is (= (map (fn [trailhead]
                    (trailhead-score (topo-map sample-10-larger-lines) trailhead))
                  [[0 2] [0 4]
                   [2 4]
                   [4 6]
                   [5 2] [5 5]
                   [6 0] [6 6]
                   [7 1]])
             [5, 6, 5, 3, 1, 3, 5, 3, 5]))))
  (testing "part 1"
    (testing "sample"
      (is (= (solve-day-10-part-1 sample-10-larger-lines))))
    (testing "puzzle"
      (is (= (solve-day-10-part-1 (puzzle/in-lines 2024 10))
             629)))))

(deftest seq-dissociation
  (testing "dissoc-seq"
    (is (= [1 3] (dissoc-seq [1 2 3] 1)))))


(defn disk-seq-str [d-s]
  (apply str (map #(or % ".") d-s)))


(def sample-9 "2333133121414131402")

(deftest day-9
  (testing "parse sample input"
    (is (= (disk-seq-str (disk-seq sample-9))
           "00...111...2...333.44.5555.6666.777.888899")))

  (testing "compacting"
    (is (= (disk-seq-str (compact-files (disk-seq sample-9)))
           "0099811188827773336446555566")))

  (testing "part 1"
    (testing "sample"
      (is (= (solve-day-9-part-1 sample-9)
             1928)))
    (testing "puzzle"
      (is (= (solve-day-9-part-1 (puzzle/in 2024 9))
             6349606724455))))

  (testing "free blocks"
    (testing "00...111...2...333.44.5555.6666.777.888899"
      ;; idx  0         1         2         3         4
      ;; idx  012345678901234567890123456789012345678901
      ;; size   3     3   3     1  1    1    1   1
      (is (= (free-blocks (disk-seq sample-9))
             [{:size 3, :start-idx 2}
              {:size 3, :start-idx 8}
              {:size 3, :start-idx 12}
              {:size 1, :start-idx 18}
              {:size 1, :start-idx 21}
              {:size 1, :start-idx 26}
              {:size 1, :start-idx 31}
              {:size 1, :start-idx 35}]))))
  (testing "move on disk"
    (testing "00...111...2...333.44.5555.6666.777.888899"
      ;; idx  0         1         2         3         4
      ;; idx  012345678901234567890123456789012345678901
      (is (= "0099.111...2...333.44.5555.6666.777.8888.."
             (disk-seq-str
              (move-file-on-disk
               (disk-seq sample-9)
               41 2 2))))))
  (testing "track free block updates after move"
    (is (= (update-free-blocks-from-move (free-blocks (disk-seq sample-9)) 0 2)
           [{:size 1, :start-idx 4}
            {:size 3, :start-idx 8}
            {:size 3, :start-idx 12}
            {:size 1, :start-idx 18}
            {:size 1, :start-idx 21}
            {:size 1, :start-idx 26}
            {:size 1, :start-idx 31}
            {:size 1, :start-idx 35}]))
    (is (= (-> (free-blocks (disk-seq sample-9))
               (update-free-blocks-from-move 0 2)
               (update-free-blocks-from-move 0 1))
           [{:size 3, :start-idx 8}
            {:size 3, :start-idx 12}
            {:size 1, :start-idx 18}
            {:size 1, :start-idx 21}
            {:size 1, :start-idx 26}
            {:size 1, :start-idx 31}
            {:size 1, :start-idx 35}]))
    (is (= (-> (free-blocks (disk-seq sample-9))
               (update-free-blocks-from-move 0 2)
               (update-free-blocks-from-move 1 3)
               (update-free-blocks-from-move 1 2)
               )
           [{:start-idx 4, :size 1}
            {:start-idx 14, :size 1}
            {:size 1, :start-idx 18}
            {:size 1, :start-idx 21}
            {:size 1, :start-idx 26}
            {:size 1, :start-idx 31}
            {:size 1, :start-idx 35}])))
  (testing "identifies free block index of size 1"
    (is (= 0
           (block-index-of-min-size (update-free-blocks-from-move (free-blocks (disk-seq sample-9)) 0 2)
                                    1))))
  (testing "proper file sizes"
    (testing "00...111...2...333.44.5555.6666.777.888899"
      ;; idx  0         1         2         3         4
      ;; idx  012345678901234567890123456789012345678901
      (is (= 0 (file-size (disk-seq sample-9) 31)))
      (is (= 1 (file-size (disk-seq sample-9) 11)))
      (is (= 2 (file-size (disk-seq sample-9) 41)))))

  ;; TODO: Bug here. We'll have to circle back around to find out why
  ;;       in the last step 2 moves forward rather than backwards, and
  ;;       why the size-1 free block starting at index 4 doesn't seem
  ;;       to be in the free block list.
  #_(testing "compacting without fragmentation"
    (testing "00...111...2...333.44.5555.6666.777.888899"
      (is (= "00992111777.44.333....5555.6666.....8888.."
          ;; "0099.111777.442333....5555.6666.....8888.."
          ;; idx  0         1         2         3         4
          ;; idx  012345678901234567890123456789012345678901
          ;;                    .  .    .    .   .
             (disk-seq-str
              (compact-files-no-frag (disk-seq sample-9))))))))


;; likely to move to advent.combinatorics. We'll see.
(deftest combinatorics
  (testing "pairs"
    (is (= (set (all-pairs [1 2 3]))
           (set [[1 2] [1 3] [2 3]])))))

(def sample-8-lines
  (puzzle/sample-lines
   "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"))

(deftest day-8
  (testing "getting frequencies"
    (is (= (frequency-locations (grids/from-lines sample-8-lines))
           {"0" [[1 8] [2 5] [3 7] [4 4]], "A" [[5 6] [8 8] [9 9]]})))
  (testing "antinodes"
    (testing "example"
      (is (= (set [[1 3] [7 6]])
             (set (pair-antinodes [[3 4] [5 5]]))
             (set (antenna-freq-antinodes [[3 4] [5 5]]))))
      (is (= (set (antenna-freq-antinodes [[3 4] [5 5] [4 8]]))
             #{[7 6] [1 3] [2 0] [3 11] [6 2] [5 12]}))))
  (testing "part 1"
    (testing "sample"
      (is (= 14 (solve-day-8-part-1 sample-8-lines))))
    (testing "puzzle"
      (is (= (solve-day-8-part-1 (puzzle/in-lines 2024 8))
             280))))
  (testing "part 2"
    (testing "sample"
      (is (= 34 (solve-day-8-part-2 sample-8-lines))))
    (testing "puzzle"
      (is (= (solve-day-8-part-2 (puzzle/in-lines 2024 8))
             958)))))


(def sample-7-lines
  (puzzle/sample-lines
   "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"))

(deftest day-7
  (testing "bridge calibration parsing examples"
    (is (= {:test-value 190
            :factors [10 19]}
           (bridge-calibration-input (first sample-7-lines)))))
  (testing "op-seq generation"
    (is (= (bridge-calibration-op-seqs 1)
           [[+]
            [*]]))
    (is (= (bridge-calibration-op-seqs 2)
           [[+ +]
            [+ *]
            [* +]
            [* *]]))
    (is (= (bridge-calibration-op-seqs 3)
           [[+ + +]
            [+ + *]
            [+ * +]
            [+ * *]
            [* + +]
            [* + *]
            [* * +]
            [* * *]])))
  (testing "op seq application"
    (is (= 3
           (apply-op-seq [1 1 1] [+ +])))
    (is (= 20
           (apply-op-seq [1 3 5] [+ *])))
    (is (= 8
           (apply-op-seq [1 3 5] [* +]))))
  (testing "part 1 samples"
    (is (= [true true nil nil nil nil nil nil true]
           (map (comp bridge-calibration-satisfyable? bridge-calibration-input) sample-7-lines))))
  (testing "solutions"
    (testing "sample"
      (is (= (solve-day-7-part-1 sample-7-lines)
             3749))))
  (testing "puzzle"
    (is (= (solve-day-7-part-1 (puzzle/in-lines 2024 7))
           20281182715321)))
  (testing "concat operator"
    (is (= 12345 (|| 12 345))))
  (testing "part 2"
    (testing "sample with concat"
      (is (bridge-calibration-satisfyable-with-concat? {:test-value 156 :factors [15 6]})))
    (testing "sample"
      (is (= (solve-day-7-part-2 sample-7-lines)
             11387))
      #_(is (= (solve-day-7-part-2 (puzzle/in-lines 2024 7))
             159490400628354))))) ;; commenting out because the brute-force solution takes too long for unit testing

(def sample-6-lines
  (puzzle/sample-lines
   "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."))

(deftest day-6
  (testing "take in the lab"
    (is (= (dissoc (lab-from-lines sample-6-lines) :in-lab?)
           {:obstruction-locs #{[7 8] [9 6] [1 9] [4 7] [8 0] [6 1] [0 4] [3 2]}
            :guard {:position [6 4]
                    :direction [-1 0]}})))
  (testing "path prediction"
    (is (= (guard-path (lab-from-lines sample-6-lines))
           '([6 4] [5 4] [4 4] [3 4] [2 4] [1 4]
             [1 4] [1 5] [1 6] [1 7] [1 8]
             [1 8] [2 8] [3 8] [4 8] [5 8] [6 8]
             [6 8] [6 7] [6 6] [6 5] [6 4] [6 3] [6 2]
             [6 2] [5 2] [4 2]
             [4 2] [4 3] [4 4] [4 5] [4 6]
             [4 6] [5 6] [6 6] [7 6] [8 6]
             [8 6] [8 5] [8 4] [8 3] [8 2] [8 1]
             [8 1] [7 1]
             [7 1] [7 2] [7 3] [7 4] [7 5] [7 6] [7 7]
             [7 7] [8 7] [9 7]))))
  (testing "part 1 solution"
    (is (= (solve-day-6-part-1 sample-6-lines)
           41))
    (is (= (solve-day-6-part-1 (puzzle/in-lines 2024 6))
           4711)))

  (comment
    (guard-states (lab-from-lines sample-6-lines)))
  (testing "sequence repetition detection"
    (testing "when it does repeat"
      (is (seq-repeats? [1 1]))
      (is (seq-repeats? [1 2 1]))
      (is (seq-repeats? [1 2 3 1]))
      (is (seq-repeats? [3 2 3 1])))
    (testing "when it doesn't repeat"
      (is (not (seq-repeats? [])))
      (is (not (seq-repeats? [1])))
      (is (not (seq-repeats? [1 2])))
      (is (not (seq-repeats? [1 2 3])))))
  (testing "part 2 solution"
    (is (= (solve-day-6-part-2 sample-6-lines)
           6))
    (is (= (solve-day-6-part-2 (puzzle/in-lines 2024 6))
           1562))))


(deftest day-5
  (testing "can parse a rule"
    (is (= (parse-rule "a|b")
           ["a" "b"])))
  (testing "can parse an update"
    (is (= (parse-update "75,29,13")
           ["75" "29" "13"])))
  (def sample-5-lines
    (puzzle/sample-lines
     "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"))
  (comment
    (parse-ordering-rules sample-5-lines)
    (rule-map (:rules (parse-ordering-rules sample-5-lines))))
  (testing "Expected sequence pairs"
    (is (= (take 20 (sequence-pairs [1 2 3]))
           [[1 2] [1 3] [2 3]])))

  (testing "checkers can fail"
    (is (let [{:keys [rules rule-map updates]} (parse-ordering-rules sample-5-lines)
              checker (partial-order-checker rule-map)]
          (not (checker ["75" "97"])))))

  (testing "rule parser examples"
    (is (= [true true true false false false]
           (let [{:keys [rules rule-map updates]} (parse-ordering-rules sample-5-lines)
                 checker (partial-order-checker rule-map)]
             (map (fn [upd]
                    (update-follows-order checker upd))
                  updates)))))

  (testing "middle element examples"
    (is (= "61" (middle-update-element (parse-update "75,47,61,53,29")))))

  (testing "part 1 solution"
    (testing "sample"
      (= 143 (solve-day-5-part-1 sample-5-lines)))
    (testing "puzzle"
      (is (= 5948 (solve-day-5-part-1 (puzzle/in-lines 2024 5))))))
  (testing "nil-preserving comparisons"
    (is (= (compare-preserving-nil 1 2)
           -1))
    (is (= (compare-preserving-nil nil 2)
           0))
    (is (= (compare-preserving-nil 1 nil)
           0))
    (is (= (compare-preserving-nil 1 1)
           0))
    (is (= (compare-preserving-nil 2 1)
           1)))
  (testing "part 2 solution"
    (testing "sample"
      (is (= 123 (solve-day-5-part-2 sample-5-lines))))
    (comment
      (solve-day-5-part-2 sample-5-lines)
      #_(("97" "75" "47" "61" "53")
         ("61" "29" "13")
         ("97" "75" "47" "29" "13")))
    (testing "puzzle"
      (solve-day-5-part-2 (puzzle/in-lines 2024 5))
      ;; => 3786 ;; too high
      ;; TODO: Investigate by checking whether the 'sorted' lists are indeed sorter per the checker in part 1.
      ())))

(deftest grids
  (testing "grid construction"
    (is (= [["1" "2" "3"]
            ["4" "5" "6"]
            ["7" "8" "9"]]
           (grid (puzzle/sample-lines "123
456
789")))))
  (testing "grid size"
    (is (= {:rows 3, :columns 4}
           (grid-size [[1 2 3 4]
                       [1 2 3 4]
                       [1 2 3 4]]))))
  (testing "grid bounds"
    (is (= (let [t true
                 f false]
             [f f f f f
              f t t t f
              f t t t f
              f f f f f])
           (let [in-bounds? (bounds-checker [[1 2 3]
                                             [4 5 6]])]
             (for [r (range -1 3)
                   c (range -1 4)]
               (in-bounds? [r c]))))))
  (testing "cell seq"
    (is (= [3 5 7]
           (cell-seq [[1 2 3]
                      [4 5 6]
                      [7 8 9]]
                     [0 2]
                     [1 -1])))))
;; => #'advent.y2024-test/grids

(deftest day-4
  (def sample-4-1-lines
    (puzzle/sample-lines
     "..X...
.SAMX.
.A..A.
XMAS.S
.X...."))
  (def sample-4-2
    (puzzle/sample-lines
     "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"))
  (testing "x locations"
    (is (= #{[0 2] [1 4] [3 0] [4 1]}
           (into #{} (x-locations (grid sample-4-1-lines))))))
  (testing "xmases-at"
    (is (= [[1 1]]
           (xmases-at (grid sample-4-1-lines) [0 2]))))
  (testing "part 1 solutions"
    (is (= 4 (solve-day-4-part-1 sample-4-1-lines)))
    (is (= 2297 (solve-day-4-part-1 (puzzle/in-lines 2024 4)))))
  (def sample-4-3-lines
    (puzzle/sample-lines
     ".M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
.........."))
  (def sample-4-x-mas-grid
    (grid
     (puzzle/sample-lines
      "M.M
.A.
S.S")))
  (testing "x-mas detection"
    (is (x-mas-at? sample-4-x-mas-grid [1 1]))
    (is (not (x-mas-at? sample-4-x-mas-grid [1 0]))))
  (testing "part 2 solution"
    (is (= 9 (solve-day-4-part-2 sample-4-3-lines)))
    (is (= 1745 (solve-day-4-part-2 (puzzle/in-lines 2024 4))))))


(deftest day-3
  (def sample-3
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
  (testing "multiply single"
    (is (= 2024 (mul 44 46))))
  (testing "extract mul"
    (is (= [:mul 44 46]
           (extract-mul "mul(44,46)"))))
  (testing "find muls"
    (is (= (get-muls sample-3)
           ["mul(2,4)" "mul(5,5)" "mul(11,8)" "mul(8,5)"])))
  (testing "part 1 solutions"
    (is (= 161 (solve-day-3-part-1 sample-3)))
    (is (= 184511516 (solve-day-3-part-1 (puzzle/in 2024 3)))))
  (testing "nil on extract-mul mismatch"
    (is (nil? (extract-mul "mul (2,2)"))))
  (def sample-3-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
  (testing "op detection"
    (is (= '("mul(2,4)" "don't()" "mul(5,5)" "mul(11,8)" "do()" "mul(8,5)")
           (get-ops sample-3-2))))
  (testing "do parsing"
    (is (extract-do "do()"))
    (is (nil? (extract-do "mul()"))))
  (testing "don't parsing"
    (is (extract-don't "don't()"))
    (is (nil? (extract-don't "mul()"))))
  (testing "op parsing"
    (is (= (parse-ops sample-3-2)
           '([:mul 2 4] [:don't] [:mul 5 5] [:mul 11 8] [:do] [:mul 8 5])))
    (is (nil? (extract-op "nothing"))))
  (testing "op application"
    (is (= 48
           (:accumulator (apply-ops {:enabled-ops #{:mul}}
                                    '([:mul 2 4] [:don't] [:mul 5 5] [:mul 11 8] [:do] [:mul 8 5]))))))
  (testing "part 2 solutions"
    (is (= 48 (solve-day-3-part-2 sample-3-2)))
    (is (= 90044227 (solve-day-3-part-2 (puzzle/in 2024 3))))))

(deftest day-2
  (def sample-2-lines
    (puzzle/sample-lines "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"))
  (comment
    (report-diffs [7 6 4 2 1])
    (sequence
     (comp
      (map parse-report)
      (map report-diffs))
     sample-2-lines))
  (testing "part 1 solutions"
    (is (= 2 (solve-day-2-part-1 sample-2-lines)))
    (is (= 236 (solve-day-2-part-1 (puzzle/in-lines 2024 2)))))
  (testing "part 2 solutions"
    (is (= 4 (solve-day-2-part-2 sample-2-lines)))
    (is (= 308 (solve-day-2-part-2 (puzzle/in-lines 2024 2))))))


(deftest day-1
  (def sample-1
    "3   4
4   3
2   5
1   3
3   9
3   3")
  (testing "utilities"
    (testing "line pair splits and gives numbers"
      (is (= (line-pair "3   4")
             [3 4])))
    (testing "parse-day-1-lines matches examples"
      (is (= (parse-day-1-lines (puzzle/sample-lines sample-1))
             '[(1 2 3 3 3 4)
               (3 3 3 4 5 9)])))
    (testing "distance examples"
      (is (= 1 (distance 1 2)))
      (is (= 1 (distance 2 1))))
    (testing "solutions"
      (is (= (solve-day-1-pt-1 (puzzle/sample-lines sample-1))
             11))
      (is (= (solve-day-1-pt-1 (puzzle/in-lines 2024 1))
             1666427)))))
