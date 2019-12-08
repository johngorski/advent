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

(def sample-2 [1,9,10,3,2,3,11,0,99,30,40,50])

(defn day2-output [noun verb]
  (get-in
   (computer/run-intcode {:pc 0 :mem (-> (vec in-2)
                                        (assoc 1 noun)
                                        (assoc 2 verb))})
   [:mem 0]))

(comment
  (->> (for [noun (range 100)
             verb (range 100)]
         [noun verb])
       (filter #(= 19690720 (day2-output (first %) (second %))))
       )

  [31 46]
  )

(comment
  (reduce-kv #(assoc %1 %2 %3) [0 0 0] [1 0 1])
  (vec (repeat 3 identity))
  )

(def in-5 (vec (map edn/read-string (string/split (slurp (io/resource "2019/5.txt")) #","))))

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

(comment
(def mem [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])
(def phases [4,3,2,1,0])

(mem->computer mem)
(map #(phased % (mem->computer mem)) phases)
(def an-amp (first (map #(phased % (mem->computer mem)) phases)))
(update an-amp :input conj 0)

(map amp-runner (map #(phased % (mem->computer mem)) phases))
(def a-runner (first (map amp-runner (map #(phased % (mem->computer mem)) phases))))
(a-runner 0)


(disj #{1} 1)
(disj #{1} 0)
(first #{1})

(concat [1 2] [3 4])
)

(defn permutations [head set-of-things]
  (if (empty? set-of-things)
    [head]
    (mapcat #(permutations (conj head %) (disj set-of-things %)) set-of-things)))

(comment
(permutations [:head] #{1 2 3})
(permutations [1] #{})
(permutations [] #{1})
(permutations [1 2] #{3})
(permutations [1] #{2 3})

(permutations [] (into #{} (range 5)))
)

;; Day 7 part 1
(apply max (map #(thrust-from in-7 %) (permutations [] (into #{} (range 5))))) ; => 199988

;; Day 7 part 2

(comment
(count (permutations [] (into #{} (range 5 10))))
(take 10 (permutations [] (into #{} (range 1000)))) ;; recursion blows stack.
(concat [1 2] [3 4])
(concat [1 2] nil)
(concat () [1])
)

;; .  prepend output from previous amp to input
;; . run until halted
;; . take its output
;; if it's finished and it's E, the output is it
;; otherwise, recur
;; if it's awaiting input, add it to the end of the list (minus its output and :halted state)

(defn run-amp-segment [prev-out amp]
  (let [advanced (-> amp
                     (update :input #(concat % prev-out))
                     computer/run-intcode)]
    [(:output advanced) (dissoc advanced :output)]))

(def sample-7-2
  {:mem [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
   :phases [9,8,7,6,5]})

(comment
(run-amp-segment [0] (phased (first (:phases sample-7-2)) (mem->computer (:mem sample-7-2))))
)

(defn amp-loop-runner [previous-output [head-amp & tail-amps] last-amp-label]
  (when head-amp
    (let [active-amp (-> head-amp
                         (update :input #(concat % previous-output))
                         computer/run-intcode)
          {:keys [output label halted]} active-amp]
      (if (and (= last-amp-label label) (= :finished halted))
        (first output)
        (recur
         output
         (if (= :awaiting-input halted)
           (concat tail-amps [(dissoc active-amp :halted :output)])
           tail-amps)
         last-amp-label)))))

(defn labeled [n cpu] (map-indexed #(assoc %2 :label %1) (repeat n cpu)))

(comment
(labeled 4 {})
(conj (conj nil 1) 2)
(map + [1 2 3] [2 1 3])
(empty? nil)
)

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

(defn soln-day-7-part-2 []
  (apply max (map #(thrust-from-amp-loop in-7 %) (permutations [] (into #{} (range 5 10))))))

;; Day 8

(defn str->digits [s]
  (map edn/read-string (string/split s #"")))

(comment
(string/split "123" #"")
(str->digits "123456789012")
)

(defn layers [w h ds]
  (partition (* w h) ds))

(comment
(layers 3 2 (str->digits "123456789012"))
)

(def in-8 (slurp (io/resource "2019/8.txt")))

;; day 8 part 1
(comment
(let [fewest-0s (apply min-key
                     #(count (filter (fn [pxl] (= 0 pxl)) %))
                     (layers 25 6 (str->digits in-8)))
      ones (count (filter #(= 1 %) fewest-0s))
      twos (count (filter #(= 2 %) fewest-0s))]
  (* ones twos))

(count (layers 25 6 (str->digits in-8)))
)

(defn parse-image
  "parse images to layers"
  [w h pxls]
  (->> (str->digits pxls)
       (layers w h)
       (map #(vec (map vec (partition w %))))
       vec))

(comment
(parse-image 3 2 "123456789012") ; => (((1 2 3) (4 5 6)) ((7 8 9)(0 1 2)))

(parse-image 2 2 "0222112222120000") ;; => (((0 2) (2 2)) ((1 1) (2 2)) ((2 2) (1 2)) ((0 0) (0 0)))

(get-in [[1]] [0 0])

(get-in (parse-image 2 2 "0222112222120000") [1  ])

(map * [2 5] [3 7])
)

(defn merge-row [top-row bottom-row]
  (vec (map #(if (= 2 %1) %2 %1) top-row bottom-row)))

(defn merge-layer [top bottom]
  (vec (map merge-row top bottom)))

(defn compile-image [image]
  (reduce merge-layer image))

(comment
(compile-image (parse-image 2 2 "0222112222120000"))
)

(defn image->ascii [[& rows]]
  (string/join "\n" (map string/join rows)))

(comment
(image->ascii (compile-image (parse-image 2 2 "0222112222120000")))

(image->ascii (compile-image (parse-image 25 6 in-8)))

;; Day 8 part 2
(println
 (apply str
        (map #(case %
                \0 " "
                \1 "*"
                %)
             (seq (image->ascii (compile-image (parse-image 25 6 in-8)))))))
)
