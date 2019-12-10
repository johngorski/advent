(ns advent.y2019-intcode
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]))

(comment
(assoc [1] 4 9) ;; => Intcode should be able to write memory out-of-bounds in the positive direction
(assoc [1] 1 9) ;; => Going one beyond doesn't trigger it, but it should be fixed either way
(get [1] 4) ;; => Intcode should read memory beyond its bounds in the positive direction as 0
)
(into [0 0 0 0 0 0 0 0] [2 2 2 2 2])
(defn decode
  "Decodes the instruction from the memory pointer, yielding update functions for cpu components."
  [{:keys [pc mem input relative-base]}]
  (let [=>     #(fn [_] %)
        deref  #(if (< (count mem) %)
                  0
                  (get mem %))
        ++     (fn [n] (partial + n))
        put    (fn [dst value]
                 (fn [memory]
                   (let [memsize (count memory)
                         m (if (< dst memsize)
                             memory
                             (into memory (repeat (- dst memsize) 0)))]
                     (assoc m dst value))))
        op     (deref pc)
        opcode (rem op 100)
        modes  (reduce-kv
                #(assoc %1 %2 %3)
                (vec (repeat 3 deref))
                (vec
                 (map #(case %
                         \0 deref
                         \1 identity
                         \2 (comp deref (++ (or relative-base 0))))
                      (reverse (seq (str (quot op 100)))))))
        param  (fn [idx]
                 ((get modes idx) (deref (+ 1 pc idx)))) ;; get param by mode, 0-indexed after pc
        params #(map param (range %))
        operator (fn [& fs]
                   {:pc (++ 4)
                    :mem (put
                          (deref (+ 3 pc))
                          ;; (param 2)
                          (apply (apply comp fs) (params 2)))})
        int<-bool #(if % 1 0)]
    (case opcode
      1 (operator +') ;; add
      2 (operator *') ;; multiply
      3 ;; store from input
      (if (empty? input)
        {:halted (=> :awaiting-input)}
        {:pc (++ 2)
         :mem (put (deref (inc pc)) (first input))
         :input rest})
      4 ;; output
      {:pc (++ 2)
       :output #(conj (or % []) (param 0))}
      5 ;; jump-if-true
      {:pc (let [[fst snd] (params 2)]
             (if (not= 0 fst)
               (=> snd)
               (++ 3)))}
      6 ;; jump-if-false
      {:pc (let [[fst snd] (params 2)]
             (if (= 0 fst)
               (=> snd)
               (++ 3)))}
      7 (operator int<-bool <) ;; less than
      8 (operator int<-bool =) ;; equals
      9
      {:pc (++ 2)
       :relative-base (=> (+ (or relative-base 0) (param 0)))}
      99 {:halted (=> :finished)} ;; finish
      {:halted (=> :fault)}))) ;; fault

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
