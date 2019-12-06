(ns advent.y2019-intcode-test
  (:require
   [advent.y2019-intcode :refer :all]
   [clojure.test :refer :all]))

(deftest intcode-computer
  (testing "passes day 5"
    (is (= 14110739 (first (:output (run-intcode {:pc 0, :input [5], :mem in-5}))))))
  (testing "day 2 samples pass"
    (is (= 3500 (first (:mem (run-intcode {:pc 0 :mem [1,9,10,3,2,3,11,0,99,30,40,50]})))))
    (is (= 2 (first (:mem (run-intcode {:pc 0 :mem [1,0,0,0,99]})))))
    (is (= 6 (get (:mem (run-intcode {:pc 0 :mem [2,3,0,3,99]})) 3)))
    (is (= 9801 (last (:mem (run-intcode {:pc 0 :mem [2,4,4,5,99]})))))
    (is (= [30,1,1,4,2,5,6,0,99] (:mem (run-intcode {:pc 0 :mem [1,1,1,4,99,5,6,0,99]})))))
  (testing "day 2 part 2's solution computes"
    (is (= 19690720 (first (:mem (run-intcode {:pc 0 :mem (-> (vec in-2) (assoc 1 31) (assoc 2 46))})))))))
  


