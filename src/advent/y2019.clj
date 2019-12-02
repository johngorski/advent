(ns advent.y2019
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn fuel [mass]
  (-> (quot mass 3) (- 2)))

(comment
  (=
   [2 2 654 33583]
   (map fuel [12 14 1969 100756])
   ))

(def in-1 (map edn/read-string (string/split (slurp (io/resource "2019/1.txt")) #"\n")))

(comment
(= 3506577 (reduce + (map fuel in-1)))
)

(defn rocket-fuel [mass]
  (letfn [(acc-fuel [acc m]
            (let [f (fuel m)]
              (if (< 0 f)
                (recur (+ f acc) f)
                acc)))]
    (acc-fuel 0 mass)))

(comment
(rocket-fuel 14)
(rocket-fuel 1969)
(rocket-fuel 100756)
)

(comment
(= 5256960 (reduce + (map rocket-fuel in-1)))
)

(def in-2 (map edn/read-string (string/split (slurp (io/resource "2019/2.txt")) #",")))

(defn parse
  "Returns a function transforming the state of memory to the next state per the current instruction."
  [[op fst snd dest]]
  (when-let [operator ({1 +, 2 *, 99 nil} op)]
    (fn [memory] (assoc memory dest (operator (get memory fst) (get memory snd))))))

(defn run-mem
  "Returns the state after running initial state mem from position pc."
  [pc mem]
  (if-let [op (parse (subvec mem pc (+ 4 pc)))]
    (recur (+ 4 pc) (op mem))
    {:memory mem
     :halted-with (get mem pc)}))

(def sample-2 [1,9,10,3,2,3,11,0,99,30,40,50])

(comment
(run-mem 0 sample-2)
((parse [1 0 0 0]) [1 0 0 0 99])
(run-mem 0 [1,0,0,0,99, 0 ,0 0])
(run-mem 0 [2,3,0,3,99 0 0 0])
(run-mem 0 [2,4,4,5,99,0 0 0])
(run-mem 0 [1,1,1,4,99,5,6,0,99 0 0 0])

(run-mem 0 (-> (vec in-2)
               (assoc 1 12)
               (assoc 2 2)
               ))
)
(comment not 644274)
(comment 8017076)

