(ns advent.y2019-intcode
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn decode
  "Decodes the instruction from the memory pointer, yielding update functions
  for the program counter (pc) and for the memory itself"
  [{:keys [pc mem input]}]
  (let [deref  #(get mem %)
        =>     (fn [lifted] (fn [_] lifted))
        ++     (fn [d] (fn [n] (+ n d)))
        op     (deref pc)
        opcode (rem op 100)
        modes  (reduce-kv
                #(assoc %1 %2 %3)
                (vec (repeat 3 deref))
                (vec
                 (map #(case %
                         \0 deref
                         \1 identity)
                      (reverse (seq (str (quot op 100)))))))
        param  (fn [idx] ((get modes idx) (+ 1 pc idx))) ;; get param by mode, 0-indexed after pc
        ]
    (case opcode
      1 ;; add
      {:pc (++ 4)
       :mem (let [fst (deref (+ 1 pc))
                  snd (deref (+ 2 pc))
                  dst (deref (+ 3 pc))]
              #(assoc % dst (+ ((get modes 0) fst) ((get modes 1) snd))))}
      2 ;; multiply
      {:pc (++ 4)
       :mem (let [fst (deref (+ 1 pc))
                  snd (deref (+ 2 pc))
                  dst (deref (+ 3 pc))]
              #(assoc % dst (* ((get modes 0) fst) ((get modes 1) snd))))}
      3 ;; store from input
      (if (empty? input)
        {:halted (=> :awaiting-input)}
        (let [[first-input & rest-input] input]
          {:pc #(+ 2 %)
           :mem (let [fst (deref (inc pc))]
                  #(assoc % fst first-input))
           :input (=> rest-input)}))
      4 ;; output
      {:pc (++ 2)
       :output (let [fst (deref (inc pc))]
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
      {:pc (++ 4)
       :mem #(let [fst ((get modes 0) (deref (+ 1 pc)))
                   snd ((get modes 1) (deref (+ 2 pc)))
                   dst (deref (+ 3 pc))]
               (assoc % dst (if (< fst snd) 1 0)))}
      8 ;; equals
      {:pc (++ 4)
       :mem #(let [fst ((get modes 0) (deref (+ 1 pc)))
                   snd ((get modes 1) (deref (+ 2 pc)))
                   dst (deref (+ 3 pc))]
               (assoc % dst (if (= fst snd) 1 0)))}
      99
      {:halted (=> :finished)}
      ;; fault
      {:halted (=> :fault)})))

(defn step
  "One step of our Intcode execution"
  [computer]
  (reduce (fn [state [component updater]] (update state component updater))
          computer
          (decode computer)))

(defn run-intcode
  "Runs Intcode computers from the given state, returning the state in which it halts."
  [computer]
  (first (filter :halted (iterate step computer))))
