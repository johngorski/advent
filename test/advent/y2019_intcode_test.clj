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
  ;; TODO: The test file could use some cleanup, too. I mean maybe? Very strong candidate for tests that should never have to change.t
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
    (is (= 8017076 (first (:mem (run-intcode {:pc 0 :mem (-> (vec y2019/in-2)
                               (assoc 1 12)
                               (assoc 2 2)
                               )})))))
    (is (= 19690720 (first (:mem (run-intcode {:pc 0 :mem (-> (vec y2019/in-2) (assoc 1 31) (assoc 2 46))}))))))
  (testing "Day 5 still works"
    (testing "passes day 5"
    (is (= 13087969 (last (:output (run-intcode {:pc 0, :input [1], :mem y2019/in-5})))))
    (is (= 14110739 (first (:output (run-intcode {:pc 0, :input [5], :mem y2019/in-5})))))))
  (testing "Day 7 still works"
    (is (= 43210 (y2019/thrust-from [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] [4,3,2,1,0])))
    (is (= 54321 (y2019/thrust-from [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0] [0,1,2,3,4])))
    (is (= 65210 (y2019/thrust-from [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                                     1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0] [1,0,4,3,2])))
    (is (= 37230 (y2019/thrust-from y2019/in-7 [0 1 2 3 4])))
    (is (= 139629729 (y2019/thrust-from-amp-loop (:mem y2019/sample-7-2) (:phases y2019/sample-7-2))))
    (is (= 18216 (y2019/thrust-from-amp-loop [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10] [9,7,8,5,6])))
    (is (= 17519904 (y2019/soln-day-7-part-2))))
  (testing "Relative base adjustment"
    (is (= 2019 (:relative-base (step {:pc 0 :mem [109,19] :relative-base 2000}))))
    (is (= 1985 (:relative-base (step {:pc 0 :mem [109,1985]})))))
;;  (+' Long/MAX_VALUE Long/MAX_VALUE)
;;  (*' Long/MAX_VALUE Long/MAX_VALUE)
  (testing "Day 9 test quine"
    (let [quine [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]]
      (is (= quine (:output (run-intcode {:pc 0 :mem quine}))))))
  (testing "Handles large numbers"
    (is (= [1219070632396864] (:output (run-intcode {:pc 0 :mem [1102,34915192,34915192,7,4,7,99,0]}))))
    (is (= [1125899906842624] (:output (run-intcode {:pc 0 :mem [104,1125899906842624,99]})))))
  (testing "put can grow memory" ;; ((put 1000 2) [])
    (is (= [9 0 0 7] ((put 3 7) [9])))
    (is (= [203 2 0 0 0 0 5] ((put 6 5) [203 2]))))
  (testing "relative move detection"
    (is (= [\2 \0 \0] (mode-codes 203))))
  (testing "relative base mode works for input"
    (is (= [203 3 0 0 0 0 5] (:mem (step {:pc 0 :relative-base 3 :input [5] :mem [203 3]})))))
  (testing "Day 9 part 1"
    (is (= [2518058886] (:output (run-intcode {:pc 0 :mem y2019/in-9 :input [1]}))))))

