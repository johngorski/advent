(ns advent.y2018-test
  (:require
   [advent.puzzle :as puzzle]
   [advent.y2018 :refer :all]
   [clojure.edn :as edn]
   [clojure.test :refer :all]))


(deftest day-1
  (def sample-1
    "")
  (testing "solution"
    (is (= 433 (solve-day-1-part-1 (puzzle/in-lines (map edn/read-string) 2018 1))))
    (is (= 256 (solve-day-1-part-2 (puzzle/in-lines (map edn/read-string) 2018 1))))))
