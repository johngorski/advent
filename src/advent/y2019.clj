(ns advent.y2019
  (:require
   [advent.y2019-intcode :as computer]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
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
      {:pc (fn [_] nil)})))

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

;; Day 3
(def sample-wire "R8,U5,L5,D3")
(defn wire-segments [wire] (string/split wire #","))

(comment (wire-segments sample-wire))

(defn segment-effect [segment]
  (let [mag     (edn/read-string (subs segment 1))
        stepper (case (first segment)
                  \L #(update % 0 dec)
                  \U #(update % 1 inc)
                  \R #(update % 0 inc)
                  \D #(update % 1 dec))]
    (repeat mag stepper)))

(comment
  ((apply comp (segment-effect "R5")) [0 0])
  (reductions (fn [point step] (step point)) [0 0] (segment-effect "R5"))
  )

(defn wire-points [wire]
  (reductions
   (fn [point step] (step point))
   [0 0]
   (mapcat segment-effect (wire-segments wire))))

(comment
  (wire-points sample-wire)
  )

(defn wire-crossings
  "Crossings between wire a and wire b, not counting the origin."
  [a b]
  (filter (into #{} (remove (partial = [0 0]) (wire-points a))) (wire-points b)))

(comment
  (wire-crossings "R8,U5,L5,D3" "U7,R6,D4,L4")
  )

(defn abs [n] (max n (- n)))

(defn manhattan-distance [point] (reduce + (map abs point)))

(comment
  (manhattan-distance [3 -4])
  )

(def in-3 (string/split (slurp (io/resource "2019/3.txt")) #"\n"))

(comment
  (let [[a b] in-3]
    (->> (wire-crossings a b)
         (map manhattan-distance)
         (apply min)))
  )
;; 245

(defn steps-to
  "Number of steps it takes to traverse from the origin to the given point"
  [point wire]
  (count (take-while #(not= % point) (wire-points wire))))

(comment
  (steps-to [5 0] "R10")
  (steps-to [-3 4] "L3,U10,R20")
  )

(comment
  (let [[a b] in-3]
    (->> (wire-crossings a b)
         (map #(+ (steps-to % a) (steps-to % b)))
         (apply min)))
  )
;; 48262

;; Day 4
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

(comment
  (has-same-adjacent? 111111)
  (has-same-adjacent? 223450)
  (has-same-adjacent? 123789)
  (<= 1 1 1 1)
  )

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
(comment
  (re-find #"[^1]11[^1]" (str 111122))
  (re-find #"(^|[^2])22([^2]|$)" (str 111122))
  (re-find #"2" (str 2222))
  )

(defn exactly-two
  "regex with exactly two of the given digits"
  [d]
  (re-pattern (str "(^|[^" d "])" d d "([^" d "]|$)")))

(defn has-double? [num]
  ((apply some-fn (map #(partial re-find %) (map exactly-two (range 10)))) (str num)))

(comment
  (has-double? 112233)
  (has-double? 123444)
  (has-double? 111122)
  )

(comment
  (count (filter has-double? (filter potential-password? (range (first in-4) (inc (second in-4))))))
  )
;; 763

;; Day 6
(defn parse-orbit [s]
  (->> (string/split s #"\n")
       (map #(string/split % #"\)"))
       (map (fn [[orbitee orbiter]] [orbiter orbitee]))
       (into {})))

(comment
(parse-orbit "C)D") ;; => ["C" "D"] -- "D orbits C"
)

(def sample-6 "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")

(comment
(parse-orbit sample-6)

(let [sample (parse-orbit sample-6)]
  (count (sort (into #{} (concat (keys sample) (vals sample))))))
)

(defn planets-in-map [m]
  (into #{} (concat (keys m) (vals m))))

(comment
(count (planets-in-map (parse-orbit sample-6)))
)

(defn orbits-for [planet m]
  (take-while some? (iterate #(get m %) (get m planet))))

(orbits-for "F" (parse-orbit sample-6))

(defn orbits-in [m]
  (reduce
   +
   (for [planet (planets-in-map m)]
     (count (orbits-for planet m)))))

(orbits-in (parse-orbit sample-6)) ;; => 4

(def in-6 (slurp (io/resource "2019/6.txt")))

(orbits-in (parse-orbit in-6)) ;; 110190

(def day-6-planet-map (parse-orbit in-6))

;; transfers will be the magnitude of the disjoint union of our paths

(defn transfers-to-santa [m]
  (let [you-planets (into #{} (orbits-for "YOU" m))
        san-planets (into #{} (orbits-for "SAN" m))]
    (count (set/difference
            (set/union you-planets san-planets)
            (set/intersection you-planets san-planets)))))

(def sample-6-2 "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN")

(comment
(transfers-to-santa (parse-orbit sample-6-2)) ;; => 4

(transfers-to-santa day-6-planet-map) ;; => 343
)

(def in-7 (vec (map edn/read-string (string/split (slurp (io/resource "2019/7.txt")) #","))))
(def cold-amplifier {:pc 0 :mem in-7})

(defn phased-amplifier [phase] (assoc cold-amplifier :input [phase]))

(defn amplifier-runner [amplifier]
  (fn [input] (computer/run-intcode (update amplifier conj input))))

(defn mem->computer [mem] {:pc 0 :mem mem})

(defn phased [phase cpu] (assoc cpu :input [phase]))

(defn amp-runner [amp]
  (fn [input]
    (first (:output (computer/run-intcode (update amp :input conj input))))))

(defn thrust-from [mem phases]
  (let [cpu     (mem->computer mem)
        amps    (map #(phased % cpu) phases)
        runners (map amp-runner amps)]
    (reduce #(%2 %1) 0 runners)))

(def mem [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])
(def phases [4,3,2,1,0])

(mem->computer mem)
(map #(phased % (mem->computer mem)) phases)
(def an-amp (first (map #(phased % (mem->computer mem)) phases)))
(update an-amp :input conj 0)

(map amp-runner (map #(phased % (mem->computer mem)) phases))
(def a-runner (first (map amp-runner (map #(phased % (mem->computer mem)) phases))))
(a-runner 0)
(thrust-from [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] [4,3,2,1,0]) ;; => 43210
(thrust-from [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0] [0,1,2,3,4]) ;; => 54321
(thrust-from [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0] [1,0,4,3,2]) ;; => 65210

;; (thrust-from in-7 [0 1 2 3 4])

(disj #{1} 1)
(disj #{1} 0)
(first #{1})

(concat [1 2] [3 4])

(defn permutations [head set-of-things]
  (if (empty? set-of-things)
    [head]
    (mapcat #(permutations (conj head %) (disj set-of-things %)) set-of-things)))
    
(permutations [:head] #{1 2 3})
(permutations [1] #{})
(permutations [] #{1})
(permutations [1 2] #{3})
(permutations [1] #{2 3})

(permutations [] (into #{} (range 5)))

;; Day 7 part 1
(apply max (map #(thrust-from in-7 %) (permutations [] (into #{} (range 5))))) ; => 199988

;; Day 7 part 2
(count (permutations [] (into #{} (range 5 10))))

(comment
(take 10 (permutations [] (into #{} (range 1000)))) ;; recursion blows stack.
)

;; .  prepend output from previous amp to input
;; . run until halted
;; . take its output
;; if it's finished and it's E, the output is it
;; otherwise, recur
;; if it's awaiting input, add it to the end of the list (minus its output and :halted state)

(concat [1 2] [3 4])
(concat [1 2] nil)
(concat () [1])

(defn run-amp-segment [prev-out amp]
  (let [advanced (-> amp
                     (update :input #(concat % prev-out))
                     computer/run-intcode)]
    [(:output advanced) (dissoc advanced :output)]))

(def sample-7-2
  {:mem [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
   :phases [9,8,7,6,5]})

(run-amp-segment [0] (phased (first (:phases sample-7-2)) (mem->computer (:mem sample-7-2))))

(defn amp-loop-runner [previous-output [head-amp & tail-amps] last-amp-label]
  (when head-amp
    (let [active-amp (-> head-amp
                         (update :input #(concat % previous-output))
                         computer/run-intcode)
          {:keys [output label halted]} active-amp]
      (if (and (= last-amp-label label) (= :finished halted))
        output
        (recur
         output
         (if (= :awaiting-input halted)
           (concat tail-amps [(dissoc active-amp :halted :output)])
           tail-amps)
         last-amp-label)))))

(defn labeled [n cpu] (map-indexed #(assoc %2 :label %1) (repeat n cpu)))
(labeled 4 {})
(conj (conj nil 1) 2)
(map + [1 2 3] [2 1 3])
(empty? nil)
(defn thrust-from-amp-loop [mem phases]
  (let [n-amps  (count phases)
        cpus    (labeled n-amps (mem->computer mem))
        last-label (dec n-amps)
        amps    (map phased phases cpus)]
    ;;(map #(dissoc % :mem) amps)))
    ;; amps))
    ;; (first amps)))
    ;; [[0] (map #(dissoc % :label :input) amps) last-label]))
    (amp-loop-runner [0] amps last-label)))

(comment
(thrust-from-amp-loop (:mem sample-7-2) (:phases sample-7-2)) ;; => 139629729

)
