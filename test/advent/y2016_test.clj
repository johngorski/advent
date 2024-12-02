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
             (take 5 (iterate turn-left north)))))
    (testing "vector addition"
      (is (= [3 4] (v+ [1 1] [2 3]))))
    (testing "forward"
      (is (= [3 3] (forward [3 4] south))))
    (testing "manhattan distance"
      (is (= 7 (manhattan-distance [0 0] [3 4])))))
  (testing "examples"
    (testing "R2, L3 position"
      (is (= [2 3]
             (-> (new-guy)
                 turn-guy-right move-guy-forward move-guy-forward
                 turn-guy-left move-guy-forward move-guy-forward move-guy-forward
                 :position
                 ))))
    (testing "R2, R2, R2 position"
      (is (= [0 -2]
             (-> (new-guy)
                 turn-guy-right move-guy-forward move-guy-forward
                 turn-guy-right move-guy-forward move-guy-forward
                 turn-guy-right move-guy-forward move-guy-forward
                 :position
                 ))))
    (testing "R5, L5, R5, R3 distance"
      (is (= 12
             (-> (new-guy)
                 turn-guy-right move-guy-forward move-guy-forward move-guy-forward move-guy-forward move-guy-forward
                 turn-guy-left move-guy-forward move-guy-forward move-guy-forward move-guy-forward move-guy-forward
                 turn-guy-right move-guy-forward move-guy-forward move-guy-forward move-guy-forward move-guy-forward
                 turn-guy-right move-guy-forward move-guy-forward move-guy-forward
                 :position
                 (manhattan-distance [0 0]
                 ))))))
  #_(testing "solution"
    (is (= 0 (solve-day-1-part-1 (puzzle/in-lines (map edn/read-string) 2018 1))))
    (is (= 0 (solve-day-1-part-2 (puzzle/in-lines (map edn/read-string) 2018 1))))))
