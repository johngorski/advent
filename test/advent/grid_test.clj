(ns advent.grid-test
  (:require
   [advent.grid :refer :all]
   [advent.puzzle :as puzzle]
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

