(ns advent.y2019-intcode-test
  (:require
   [advent.y2019 :as y2019]
   [advent.y2019-intcode :refer :all]
   [clojure.test :refer :all]))

(deftest intcode-computer
  (testing "mode samples"
    (is (= 99 (last (:mem (run-intcode {:pc 0 :mem [1002,4,3,4,33]})))))
    (is (= 99 (last (:mem (run-intcode {:pc 0 :mem [1101,100,-1,4,0]}))))))
  (testing "intcode i/o"
    (is (= :the-input (first (:output (run-intcode {:pc 0 :mem [3,0,4,0,99] :input [:the-input]}))))))
  (testing "program using position mode to output 1 iff input = 8"
    (is (= 1 (first (:output (run-intcode {:pc 0, :mem [3,9,8,9,10,9,4,9,99,-1,8], :input [8]})))))
    (is (= 0 (first (:output (run-intcode {:pc 0, :mem [3,9,8,9,10,9,4,9,99,-1,8], :input [7]}))))))
  (testing "position mode for less than"
    (letfn [(less-than-8 [input] (first (:output (run-intcode {:pc 0, :mem [3,9,7,9,10,9,4,9,99,-1,8], :input [input]}))))]
      (do
        (is 1 (less-than-8 7))
        (is 0 (less-than-8 8))
        (is 0 (less-than-8 9)))))
  ;; TODO: immediate mode tests for equal and less than; position+immediate jump tests; larger day 5 part 2 sample.
  ;; TODO: The test file could use some cleanup, too. I mean maybe? Very strong candidate for tests that should never have to change.
  ;; Refactoring these would be an intellectual exercise only. Or Capernaum use case. Who knows?
  )

(deftest halting-behavior
  (testing "Halts when input needed"
    (is (= :awaiting-input (:halted (run-intcode {:pc 0 :mem [3 1]}))))
    (is (= :awaiting-input (:halted (run-intcode {:pc 0 :mem [3 1] :input []})))))
  (testing "Halts on termination"
    (is (= :finished (:halted (run-intcode {:pc 0 :mem [99]})))))
  (testing "Halts on bad opcode"
    (is (= :fault (:halted (run-intcode {:pc 0 :mem [-1]}))))))

(deftest advent-programs
  (testing "Day 2 still works"
    (testing "day 2 samples pass"
    (is (= 3500 (first (:mem (run-intcode {:pc 0 :mem [1,9,10,3,2,3,11,0,99,30,40,50]})))))
    (is (= 2 (first (:mem (run-intcode {:pc 0 :mem [1,0,0,0,99]})))))
    (is (= 6 (get (:mem (run-intcode {:pc 0 :mem [2,3,0,3,99]})) 3)))
    (is (= 9801 (last (:mem (run-intcode {:pc 0 :mem [2,4,4,5,99]})))))
    (is (= [30,1,1,4,2,5,6,0,99] (:mem (run-intcode {:pc 0 :mem [1,1,1,4,99,5,6,0,99]}))))))
  (testing "day 2 part 2's solution computes"
    (is (= 8017076 (first (:mem (run-intcode {:pc 0 :mem (-> (vec in-2)
                               (assoc 1 12)
                               (assoc 2 2)
                               )})))))
    (is (= 19690720 (first (:mem (run-intcode {:pc 0 :mem (-> (vec in-2) (assoc 1 31) (assoc 2 46))}))))))
  (testing "Day 5 still works"
    (testing "passes day 5"
    (is (= 13087969 (last (:output (run-intcode {:pc 0, :input [1], :mem in-5})))))
    (is (= 14110739 (first (:output (run-intcode {:pc 0, :input [5], :mem in-5})))))))
  (testing "Day 7 still works"
    ))
