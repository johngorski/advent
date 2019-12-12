(ns advent.y2019
  (:require
   [advent.y2019-intcode :as computer]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as sets]
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
    (count (sets/difference
            (sets/union you-planets san-planets)
            (sets/intersection you-planets san-planets)))))

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

(def in-9 (vec (map edn/read-string (string/split (slurp (io/resource "2019/9.txt")) #","))))

;; Day 10

;; Part 1: Thought: Compute slope + left/right + up/down between each asteroid and all the others
;;         and throw into set. Set with highest cardinality is our outpost.

(defn asteroid-slope
  "direction asteroid b is from asteroid a"
  [[ax ay] [bx by]]
  (let [rise (- ay by)
        run  (- bx ax)]
    (if (= 0 run)
      {:slope :!
       :direction (if (< rise 0) :below :above)}
      {:slope (/ rise run)
       :direction (if (< run 0) :left :right)})))

(asteroid-slope [0 0] [0 1]) ;; => {:slope :!, :direction :below}
(asteroid-slope [0 1] [0 0]) ;; => {:slope :!, :direction :above}
(asteroid-slope [3 4] [8 6]) ;; => {:slope -2/5, :direction :left}
(asteroid-slope [0 1] [1 0]) ;; => {:slope 1, :direction :right}



;; Bedtime. Parse asteroid file and find the max slopes of an asteroid field tomorrow.

(def sample-10-1
  ".#..#
.....
#####
....#
...##")

(defn rows [s]
  (string/split s #"\n"))

(rows sample-10-1)

(defn idx-cols [row]
  (->> row
       seq
       (map-indexed vector)))

(idx-cols (first (rows sample-10-1)))

(defn asteroids [asteroid-field-str]
  (->> asteroid-field-str
       rows
       (map-indexed (fn [r-idx row]
                      (->> (idx-cols row)
                           (keep (fn [[c-idx col]]
                                   (when (= col \#)
                                     [c-idx r-idx])))
                           seq)))
       concat
       (apply concat)
       set))

(asteroids sample-10-1)
(some? [1 2])
(empty? [])
(seq [])
(seq [1 2])

(defn visible-from [a a-set]
  (->> (disj a-set a)
       (map (partial asteroid-slope a))
       set
       count))

(defn max-visible [as]
  (apply max (map #(visible-from % as) as)))

(max-visible (asteroids sample-10-1)) ;; => 8

(defn puzzle-in [day] (slurp (io/resource (str "2019/" day ".txt"))))

(asteroids (puzzle-in 10))

(max-visible (asteroids (puzzle-in 10))) ;; => 269

(let [asteroid-field (asteroids (puzzle-in 10))
      sees (fn [asteroid] (visible-from asteroid asteroid-field))]
  (apply (partial max-key sees) asteroid-field))

(defn best-station [asteroid-field]
  (let [sees (fn [asteroid] (visible-from asteroid asteroid-field))]
    (apply (partial max-key sees) asteroid-field)))

(best-station (asteroids sample-10-1)) ;; => [3 4]

(best-station (asteroids (puzzle-in 10))) ;; [13 17]

(visible-from [13 17] (asteroids (puzzle-in 10)))

;; Laser portion: from the best station, merge a map of the other asteroids based
;; on their asteroid-slope from the station. Merge with a function that inserts
;; by any geometrical distance metric.

(defn manhattan [[ax ay] [bx by]]
  (+ (abs (- bx ax)) (abs (- by ay))))

;; Let's call it a vaporization table.

(defn map-values
  "Map where all keys are from m and corresponding values are (f (m k))"
  [f m]
  (into {} (for [k (keys m)
                 v (f (get m k))]
             [k v])))


(keys {:a 1, :b 2})

(defn map-values [f m]
  (reduce #(update %1 %2 f) m (keys m)))

(map-values even? {:a 1, :b 2})

(comment
(defn asteroids-by-direction

  [station asteroid-field]
  (->>
   (for [asteroid (disj asteroid-field station)]
     {(asteroid-slope station asteroid) #{asteroid}})
   (apply merge-with sets/union)))

(defn asteroids-by-direction

  [station asteroid-field]
  (->>
   (disj asteroid-field station)
   (map (juxt #(asteroid-slope station %) identity))
   (map (fn [[{:keys [direction slope]} coordinates]]
          {direction {slope #{coordinates}}}))
   (apply merge-with sets/union)))
)

;; Crap: This is a group-by! That was a fun 90 minutes.
(defn asteroids-by-direction
  "Given the station and asteroid field, returns a map first keyed with direction, then by slope to a set of asteroid coordinates"
  [station asteroid-field]
  (group-by #(asteroid-slope station %) (disj asteroid-field station)))

(asteroids-by-direction [8 3] (asteroids sample-10-2))

(def directions [:above :right :below :left])


(defn vaporization-table
  "A map from direction to a map from slope to distance."
  [station asteroid-field]
  (reduce #(%2 %1) {}
          (for [[{:keys [direction slope]} asteroids] (asteroids-by-direction station asteroid-field)]
            #(assoc-in % [direction slope] (sort-by (fn [asteroid] (manhattan station asteroid)) asteroids)))))

(manhattan [8 3] [8 0]) ;; => 3;
(manhattan [8 3] [8 1]) ;; => 2;

(vaporization-table [8 3] (asteroids sample-10-2))

(def sample-10-2
  ".#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....X...###..
..#.#.....#....##")

;; The asteroid vaporization sequence follows the previously vaporized asteroid
;; clockwise, nearest asteroid going first.

;; Only the nearest asteroid is vaporized. This is a single asteroid before moving
;; on in the case of :above and :below, and the sequence of first-by-distance
;; asteroids in order of decreasing slope for :right and increasing slope for :left.

(defn next-above
  "Seq of the next asteroids above to be vaporized before moving the laser to the right."
  [vt]
  [(first (get-in vt [:above :!]))])

(defn next-below
  "Seq of the next asteroids below to be vaporized before moving the laser to the left."
  [vt]
  [(first (get-in vt [:below :!]))])

(defn rest-above
  "Remaining asteroids above after head is vaporized"
  [vt]
  (update-in vt [:above :!] rest))

(defn rest-below
  "Remaining asteroids below after head is vaporized"
  [vt]
  (update-in vt [:below :!] rest))

(defn next-right
  "Seq of the next asteroids right to be vaporized before moving the laser below."
  [vt]
  (let [rgt (:right vt)]
    (->> rgt
         keys
         (sort-by #(- %))
         (map (fn [k] (get rgt k))))))

(defn next-left
  [{:keys [left]}]
  (->> left
       keys
       sort
       (map #(get left %))))

(defn rest-dir
  "Remaining vaporization table once the next round from the given quadrant is vaporized."
  [dir vt]
  (update vt dir #(map-values rest %)))

(rest-below {:below {:! [1 2]}})
(rest-dir :below {:below {:! [1 2]}})
(map-values rest {:! [1 2]})
(update {:below {:! [1 2]}} :below #(map-values rest %))
(rest-dir :right {:right {1 [2 3], 4 [5 6]}, :left {7 [8 9]}})
(rest nil)

;; After the quadrant seq is exhausted through the first layer, it moves to the next.
;; The vaporized asteroids are, of course, vaporized.
;; Directions cycle as in this example:
(take 8 (cycle directions))

(def clockwise-successor
  {:above :right
   :right :below
   :below :left
   :left :above})

;; The sequence starts with the asteroid above.

(defn laser-seq
  [dir vt]
 )
