(ns advent.y2019-intcode
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]))


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

(comment
  (output 12 2)
  )
(comment
  (->> (for [noun (range 100)
             verb (range 100)]
         [noun verb])
       (filter #(= 19690720 (output (first %) (second %))))
       )

  [31 46]
  )

(comment
  (reduce-kv #(assoc %1 %2 %3) [0 0 0] [1 0 1])
  (vec (repeat 3 identity))
  )

(defn decode
  "Decodes the instruction from the memory pointer, yielding update functions
  for the program counter (pc) and for the memory itself"
  [{:keys [pc mem input]}]
  (let [op     (get mem pc)
        opcode (rem op 100)
        deref  #(get mem %)
        modes  (reduce-kv
                #(assoc %1 %2 %3)
                (vec (repeat 3 deref))
                (vec
                 (map #(case %
                         \0 deref
                         \1 identity)
                      (reverse (seq (str (quot op 100)))))))]
    (case opcode
      1 ;; add
      {:pc #(+ 4 %)
       :mem (let [fst (get mem (+ 1 pc))
                  snd (get mem (+ 2 pc))
                  dst (get mem (+ 3 pc))]
              #(assoc % dst (+ ((get modes 0) fst) ((get modes 1) snd))))}
      2 ;; multiply
      {:pc #(+ 4 %)
       :mem (let [fst (get mem (+ 1 pc))
                  snd (get mem (+ 2 pc))
                  dst (get mem (+ 3 pc))]
              #(assoc % dst (* ((get modes 0) fst) ((get modes 1) snd))))}
      3 ;; store from input
      (let [[first-input & rest-input] input]
        {:pc #(+ 2 %)
         :mem (let [fst (get mem (inc pc))]
                #(assoc % fst first-input))
         :input (fn [_] rest-input)})
      4 ;; output
      {:pc #(+ 2 %)
       :output (let [fst (get mem (inc pc))]
                 #(conj (or % []) ((get modes 0) fst)))}
      5 ;; jump-if-true
      {:pc (let [fst ((get modes 0) (deref (+ 1 pc)))
                 snd ((get modes 1) (deref (+ 2 pc)))]
             #(if (not= 0 fst)
                snd
                (+ 3 %)))}
      6 ;; jump-if-false
      {:pc (let [fst ((get modes 0) (deref (+ 1 pc)))
                 snd ((get modes 1) (deref (+ 2 pc)))]
             #(if (= 0 fst)
                snd
                (+ 3 %)))}
      7 ;; less than
      {:pc #(+ 4 %)
       :mem #(let [fst ((get modes 0) (deref (+ 1 pc)))
                   snd ((get modes 1) (deref (+ 2 pc)))
                   dst (deref (+ 3 pc))]
               (assoc % dst (if (< fst snd) 1 0)))}
      8 ;; equals
      {:pc #(+ 4 %)
       :mem #(let [fst ((get modes 0) (deref (+ 1 pc)))
                   snd ((get modes 1) (deref (+ 2 pc)))
                   dst (deref (+ 3 pc))]
               (assoc % dst (if (= fst snd) 1 0)))}
      99
      {:halted (fn [_] true)})))

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
  (if-not (:halted computer)
    (recur (step computer))
    computer))

(def in-5 (vec (map edn/read-string (string/split (slurp (io/resource "2019/5.txt")) #","))))

;; Day 5
(comment
  (run-intcode {:pc 0 :mem [3,0,4,0,99] :input [69]}) ;; nice
  (run-intcode {:pc 0 :mem [1002,4,3,4,33]})
  (:output (run-intcode {:pc 0, :input [1], :mem in-5}))
  )
;; 13087969

(:output (run-intcode {:pc 0, :mem [3,9,8,9,10,9,4,9,99,-1,8], :input [8]}))
(run-intcode {:pc 0, :mem [3,9,8,9,10,9,4,9,99,-1,8], :input [8]})
(:output (run-intcode {:pc 0, :mem [3,9,8,9,10,9,4,9,99,-1,8], :input [7]}))
(run-intcode {:pc 0, :mem [3,9,8,9,10,9,4,9,99,-1,8], :input [7]})


(comment
  (:output (run-intcode {:pc 0, :input [5], :mem in-5}))
  ;; 14110739
  )
;; end day 5

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
