(ns advent.y2024-test
  (:require
   [advent.puzzle :as puzzle]
   [advent.y2024 :refer :all]
   [clojure.test :refer :all]))


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
    (is (= 2297 (solve-day-4-part-1 (puzzle/in-lines 2024 4))))))



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
