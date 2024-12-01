(ns advent.y2016-test
  (:require
   [advent.puzzle :as puzzle]
   [advent.y2016 :refer :all]
   [clojure.edn :as edn]
   [clojure.test :refer :all]))


(deftest day-1
  (def sample-1
    "")
  (testing "utilities"
    (testing "right turns"
      (is (= [north east south west north]
             (take 5 (iterate turn-right north)))))
    (testing "left turns"
      (is (= [north west south east north]
             (take 5 (iterate turn-left north))))))
  #_(testing "solution"
    (is (= 0 (solve-day-1-part-1 (puzzle/in-lines (map edn/read-string) 2018 1))))
    (is (= 0 (solve-day-1-part-2 (puzzle/in-lines (map edn/read-string) 2018 1))))))
