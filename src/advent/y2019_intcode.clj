(ns advent.y2019-intcode
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]))

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
      (if (empty? input)
        {:halted :awaiting-input}
        (let [[first-input & rest-input] input]
          {:pc #(+ 2 %)
           :mem (let [fst (get mem (inc pc))]
                  #(assoc % fst first-input))
           :input (fn [_] rest-input)}))
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
      {:halted (fn [_] :finished)})))

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

(def in-2 (map edn/read-string (string/split (slurp (io/resource "2019/2.txt")) #",")))
(def in-5 (vec (map edn/read-string (string/split (slurp (io/resource "2019/5.txt")) #","))))
