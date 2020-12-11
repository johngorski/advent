(ns advent.y2020-cpu
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.spec.alpha :as spec]
   [instaparse.core :as insta]
   [advent.y2020 :refer [puzzle-in]]))

(comment
  "day 8"

  (-> (puzzle-in 8) (string/split #"\n"))

  (edn/read-string "+113")
  ;; => 113

  (defmulti rd first)

  (defmethod rd :program [[_ & instructions]]
    (mapv rd instructions))

  (defmethod rd :instruction [[_ operation _ argument]]
    (map rd [operation argument]))

  (defmethod rd :operation [[_ operation]]
    operation)

  (defmethod rd :argument [[_ argument]]
    (edn/read-string argument))

  (defn read-program [text]
    (let [parser
          (insta/parser
           "program = instruction*
          instruction = operation #'\\s+' argument '\\n'
          operation = 'acc' | 'jmp' | 'nop'
          argument = #'[+-]\\d+'")]
      (rd (parser text))))

  (def day8 (puzzle-in 8))

  (defn decode [[operation argument]]
    (case operation
      "acc" {:accumulator (partial + argument)
             :pc inc}
      "jmp" {:pc (partial + argument)}
      "nop" {:pc inc}))

  (defn step [prg]
    (fn [{:keys [pc accumulator] :as cpu}]
      (let [instruction (get prg pc)
            effect (decode instruction)]
        (merge-with #(%1 %2) effect cpu))))

  (def prg (read-program day8))

  (take 10 (iterate (step prg) {:accumulator 0 :pc 0}))

  (def step-8 (step prg))

  (defn before-repeat [{:keys [pc accumulator] :as cpu} seen]
    (if (seen pc)
      accumulator
      (recur (step-8 cpu) (conj seen pc))))

  (before-repeat {:pc 0 :accumulator 0} #{})
  ;; => 1475

  ;; For part 2:
  ;;  - lazy seq of all prg jmp<->nop mutations
  ;;  - map anything that repeats or goes more than one past the end of the input to nil, else to the final accumulator value
  ;;  - first filter some?

  (count prg)
  (last prg)

  (def variations
    (->> (range (count prg))
         (filter #(#{"jmp" "nop"} (first (get prg %))))
         (map (fn [idx]
                (let [[op arg] (get prg idx)]
                  (update prg idx (fn [[]] [({"jmp" "nop", "nop" "jmp"} op) arg]))))
              )))

  (defn run-through
    "Runs prg on cpu until an instruction repeats ((:pc prg) has already executed),
  pc jumps way outside memory, or
  pc successfully halts by reaching (count prg).
  returns accumulator upon successful halt, nil otherwise."
    [stepper {:keys [accumulator pc] :as cpu} seen]
    (cond
      (or
       (seen pc)
       (< pc 0)
       (< (count prg) pc))
      nil

      (= (count prg) pc)
      accumulator

      :else
      (recur stepper (stepper cpu) (conj seen pc))))


  (defn try-out [prg]
    (run-through (step prg) {:accumulator 0, :pc 0} #{}))

  (try-out prg)

  (try-out (first variations))

  (first (filter some? (map try-out variations)))
  ;; => 1270
  )
