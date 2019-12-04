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

(defn output [noun verb]
  (get-in
   (run-mem 0 (-> (vec in-2)
                  (assoc 1 noun)
                  (assoc 2 verb)
                  ))
   [:memory 0]))

(output 12 2)
(comment
  (->> (for [noun (range 100)
             verb (range 100)]
         [noun verb])
       (filter #(= 19690720 (output (first %) (second %))))
       )

  [31 46]
  )

(defn decode
  "Decodes the instruction from the memory pointer, yielding update functions
  for the program counter (pc) and for the memory itself"
  [{:keys [pc mem]}]
  (let [opcode (get mem pc)]
    (case opcode
      1
      {:pc #(+ 4 %)
       :mem (let [fst (get mem (+ 1 pc))
                  snd (get mem (+ 2 pc))
                  dst (get mem (+ 3 pc))]
              #(assoc % dst (+ (get % fst) (get % snd))))}
      2
      {:pc #(+ 4 %)
       :mem (let [fst (get mem (+ 1 pc))
                  snd (get mem (+ 2 pc))
                  dst (get mem (+ 3 pc))]
              #(assoc % dst (* (get % fst) (get % snd))))}
      99
      {:pc (fn [_] nil)}
      )))

(defn step
  "One step of our Intcode execution"
  [computer]
  (reduce
   (fn [state [component updater]] (update state component updater))
   computer
   (decode computer)))

(defn run-intcode
  "Runs Intcode programs, continuously updated starting from the Day 2 puzzle."
  [computer]
  (if (:pc computer)
    (recur (step computer))
    computer))

(comment
  (run-intcode {:pc 0 :mem (vec in-2)})
  (run-intcode {:pc 0
                :mem (-> (vec in-2)
                         (assoc 1 31)
                         (assoc 2 46))})
  (step {:pc 0
         :mem (-> (vec in-2)
                  (assoc 1 31)
                  (assoc 2 46))})
  )
(comment
  (run-intcode {:pc 0 :mem sample-2})
  (run-intcode {:pc 0 :mem [1,0,0,0,99]})
  (run-intcode {:pc 0 :mem [2,3,0,3,99]})
  (run-intcode {:pc 0 :mem [2,4,4,5,99]})
  (run-intcode {:pc 0 :mem [1,1,1,4,99,5,6,0,99]})

  (run-intcode {:pc 0 :mem (-> (vec in-2)
                               (assoc 1 12)
                               (assoc 2 2)
                               )})

  (run-intcode {:pc 0 :mem (-> (vec in-2)
                               (assoc 1 31)
                               (assoc 2 46))})
  )

;; TODO: Day 3

(def in-4 [235741 706948])

(defn digits
  [num]
  (map edn/read-string (-> num str (string/split #""))))

(comment
  (digits 123456) ;; should be [1 2 3 4 5 6]
  )

(comment
  (partition 2 1 [1 2 3 4])
  )

(defn has-same-adjacent?
  "whether two adjacent digits in the number are the same"
  [num]
  (let [ds (digits num)]
    (some (partial apply =) (partition 2 1 ds))))

(has-same-adjacent? 111111)
(has-same-adjacent? 223450)
(has-same-adjacent? 123789)

(<= 1 1 1 1)

(defn digits-never-decrease? [num]
  (apply <= (digits num)))

(comment
(digits-never-decrease? 111111)
(digits-never-decrease? 223450)
(digits-never-decrease? 123789)
)

(defn potential-password?
  "Could it match a potential password from day 4, part 1?"
  [num]
  ((every-pred #(some (partial apply =) (partition 2 1 %)) ;; has-same-adjacent?
               (partial apply <=)) ;; digits-never-decrease?
   (digits num)))

(comment
(count (filter potential-password? (range (first in-4) (inc (second in-4)))))
)
;; 1178

(defn no-triples? [ds]
  (not (some (partial apply =) (partition 3 1 ds))))

(comment
(no-triples? (digits 112233))
(no-triples? (digits 123444))
(no-triples? (digits 111122)) ;; no-triples? isn't what we want, since 22 should pass this
)

(re-find #"[^1]11[^1]" (str 111122))
(re-find #"[^2]22[^2]" (str 111122))
(re-find #"2" (str 2222))

(def narrower-potential-password?
  (every-pred #(some (partial apply =) (partition 2 1 %)) ;; has-same-adjacent?
              (partial apply <=)
              no-triples?))
  
(partition 4 1 [1 2 3 4 5 6])
