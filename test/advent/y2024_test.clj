(ns advent.y2024-test
  (:require
   [advent.puzzle :as puzzle]
   [advent.y2024 :refer :all]
   [clojure.test :refer :all]))


(deftest day-6
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
           4711))))


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
    (is (= #{[0 2] [1 4] [3 0] [4 1]})
        (into #{} (x-locations (grid sample-4-1-lines)))))
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
