(ns advent.y2024-test
  (:require
   [advent.puzzle :as puzzle]
   [advent.y2024 :refer :all]
   [clojure.test :refer :all]))


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
