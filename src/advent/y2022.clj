(ns advent.y2022
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as sets]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [instaparse.core :as insta]))

(defn in [day]
  (map edn/read-string
       (string/split-lines (slurp (io/resource (str "2022/" day ".txt"))))))

(defn in-lines [day]
  (string/split-lines (slurp (io/resource (str "2022/" day ".txt")))))

;; Day 12

(def sample-12 (string/split "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi" #"\n"))

(defn ch-height [ch]
  (- (int (get {\S \a \E \z} ch ch)) (int \a)))

(comment
  (map ch-height "banana")
  ;; => (1 0 13 0 13 0)
  (map ch-height "ESa")
  ;; => (25 0 0)
  )

(defn can-step? [from to]
  (<= (ch-height to) (inc (ch-height from))))

(comment
  (can-step? \z \E)
  ;; => true
  (can-step? \z \S)
  (can-step? \S \a)
  ;; => false
  (can-step? \S \z)
  (can-step? \a \c)
  ;; => false
  (can-step? \m \n)
  ;; => true
  (can-step? \m \o)
  ;; => false
  (get-in ["apple" "banana"] [0 1])
  ;; => \p
  )
;; so in-lines loads us a ready-made heightmap....

(defn height-at [heightmap [row col]]
  (ch-height (get-in heightmap [row col] Integer/MAX_VALUE)))

(comment
  (height-at sample-12 [0 0])
  ;; => 0
  (ch-height \S)
  ;; => 0
  (height-at sample-12 [0 1])
  ;; => 0
  (height-at sample-12 [1 0])
  ;; => 0
  (height-at sample-12 [2 5])
  ;; => 25
  )

(defn heightmap-start [heightmap]
  (first
   (for [row (range (count heightmap))
         col (range (count (first heightmap)))
         :when (= \S (get-in heightmap [row col]))]
     [row col])))

(comment
  (heightmap-start sample-12)
  ;; => [0 0]
  (heightmap-start (in-lines 12))
  ;; => [20 0]
  )

(defn can-reach? [heightmap from to]
  (and
   (<= 0 (first to) (dec (count heightmap)))
   (<= 0 (second to) (dec (count (first heightmap))))
   (can-step? (get-in heightmap from Integer/MAX_VALUE)
              (get-in heightmap to Integer/MAX_VALUE))))

(comment
  (can-reach? sample-12 [2 2] [2 3])
  ;; => false
  (can-reach? sample-12 [2 2] [2 1])
  ;; => true

  (int (get-in sample-12 [-1 0] Integer/MAX_VALUE))
  ;; => 2147483647
  )

(def up [-1 0])
(def down [1 0])
(def right [0 1])
(def left [0 -1])

(defn heightmap-neighbors [heightmap from]
  (let [[row col] from]
    (sequence
     (comp
      (map (fn [[dr dc]]
             [(+ row dr) (+ col dc)]))
      (filter (fn [to]
                (can-reach? heightmap from to)))
      )
     [up down left right])))

(comment
  (heightmap-neighbors sample-12 [2 2])
  ;; => ([1 2] [3 2] [2 1])
  (heightmap-neighbors sample-12 [0 0])
  ;; => ([1 0] [0 1])
  (can-reach? sample-12 [0 0] [-1 0])
  ;; => false
  )

(defn next-paths [heightmap visited path]
  (sequence
   (comp
    (remove visited)
    (map (fn [neighbor] (conj path neighbor))))
   (heightmap-neighbors heightmap (last path))))

(comment
  (next-paths sample-12 #{} [[0 0]])
  ;; => ([[0 0] [1 0]] [[0 0] [0 1]])
  (next-paths sample-12 #{[0 0]} [[0 0] [1 0]])
  ;; => ([[0 0] [1 0] [2 0]] [[0 0] [1 0] [1 1]])
  (next-paths sample-12 #{[0 0]} [[0 0] [0 1]])
  ;; => ([[0 0] [0 1] [1 1]] [[0 0] [0 1] [0 2]])
  )

(defn find-end [heightmap]
  (loop [visited #{}
         paths [[(heightmap-start heightmap)]]]
    (or
     (first
      (filter
       (fn [path] (= \E (get-in heightmap (last path))))
       paths))
     (and (empty? paths) :no-path-found)
     (let [visited' (into visited (map last paths))]
       (recur
        visited'
        (mapcat (fn [path] (next-paths heightmap visited' path)) paths)
        )))))

(comment
  (find-end sample-12)
  [[0 0] [1 0]
   [1 1] [2 1]
   [3 1] [3 2]
   [4 2] [4 3] [4 4] [4 5] [4 6] [4 7]
   [3 7] [2 7] [1 7] [0 7]
   [0 6] [0 5] [0 4] [0 3]
   [1 3] [2 3] [3 3]
   [3 4] [3 5]
   [2 5]]
  (dec (count (find-end sample-12))))

(comment
  ;; Seems to take a while to run, let's check this again.
  ;; Probably deadlocked somewhere.
  (dec (count (find-end (in-lines 12)))))

;; Day 11

(def sample-11 (string/split "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1" #"\n"))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn monkey-id [line]
  (edn/read-string (second (re-matches #"Monkey (\d+):" line))))

(comment
  (monkey-id "Monkey 3:")
  ;; => 3
  (monkey-id "Monkey 13:")
  ;; => 13
  )

(defn monkey-starting-items [line]
  (let [[_ list-str] (re-matches #"\s*Starting items: (.+)" line)]
    (queue (map edn/read-string (string/split list-str #", ")))))

(comment
  (seq (conj (queue [1 2]) 3))
  ;; => (1 2 3)
  (seq (pop (conj (queue [1 2]) 3)))
  ;; => (2 3)
  (seq (starting-items-line "  Starting items: 77, 69, 76, 77, 50, 58"))
  ;; => (77 69 76 77 50 58)
  (seq (starting-items-line "  Starting items: 53"))
  ;; => (53)
  (class (starting-items-line "  Starting items: 53"))
  ;; => clojure.lang.PersistentQueue
  (-> (starting-items-line "  Starting items: 53") peek)
  ;; => 53
  (-> (starting-items-line "  Starting items: 53") pop seq)
  ;; => nil
  (-> (starting-items-line "  Starting items: 77, 69, 76, 77, 50, 58") pop seq)
  ;; => (69 76 77 50 58)
  (-> (starting-items-line "  Starting items: 53")))

(defn monkey-op [line]
  (let [[_ op arg] (re-matches #"\s*Operation: new = old ([+-\\*/]) (\w+)" line)
        f (symbol op)
        val (edn/read-string arg)]
    ;; [f val]
    (fn [old]
      ((eval f) old (if (= val 'old)
                      old
                      val)))
    ))

(comment
  ((monkey-op "  Operation: new = old + 8") 7)
  ;; => ["+" "8"]
  ;; => 15
  ((monkey-op "  Operation: new = old * old") 8)
  ;; => ["*" "old"]
  ;; => 64
  )

(defn monkey-test [line]
  (let [[_ factor-str] (re-matches #"\s*Test: divisible by (\d+)" line)
        factor (edn/read-string factor-str)]
    ;; factor
    (fn [worry] (zero? (rem worry factor)))
    ))

(comment
  (map (monkey-test "  Test: divisible by 3") (range 7))
  ;; => (true false false true false false true)
  )

(defn consequent-monkey [line]
  (edn/read-string (second (re-matches #"\s*If true: throw to monkey (\d+)" line))))

(comment
  (consequent-monkey "    If true: throw to monkey 4")
  ;; => 4
  )

(defn alternative-monkey [line]
  (edn/read-string (second (re-matches #"\s*If false: throw to monkey (\d+)" line))))

(comment
  (alternative-monkey "    If false: throw to monkey 2")
  ;; => 2
  )

(defn lines->monkey [lines]
  (let [[monkey-line
         starting-items-line
         op-line
         test-line
         consequent-line
         alternative-line]
        lines

        test (monkey-test test-line)
        consequent (consequent-monkey consequent-line)
        alternative (alternative-monkey alternative-line)
        ]
    {:id (monkey-id monkey-line)
     :items (monkey-starting-items starting-items-line)
     :operation (monkey-op op-line)
     :throw-to (fn [worry] (if (test worry) consequent alternative))
     :inspected-items 0}))

(defn load-monkeys [lines]
  (mapv lines->monkey
        (remove #(= 1 (count %))
                (partition-by (fn [line] (empty? line)) lines))))

(defn show-monkeys [monkeys]
  (map (fn [{:keys [id items inspected-items]}]
         {:id id
          :items (seq items)
          :inspected-items inspected-items})
       monkeys))

(comment
  (show-monkeys (load-monkeys sample-11))
  ({:id 0, :items (79 98)}
   {:id 1, :items (54 65 75 74)}
   {:id 2, :items (79 60 97)}
   {:id 3, :items (74)}))

(defn monkey-throw
  [{:keys [monkeys whose-turn]
    :as state}]
  (let [{:keys [items operation throw-to]} (monkeys whose-turn)
        worry (peek items)
        items' (pop items)
        ;; worry' (quot (operation worry) 3) ;; <-- from Part 1
        ;; worry' (rem (operation  worry) (* 23 19 13 17)) ;; <-- from Part 2 sample
        worry' (rem (operation  worry) (* 5 17 2 7 3 11 13 19)) ;; <-- from Part 2 solution
        to-monkey (throw-to worry')
        whose-turn' (if (empty? items')
                      (rem (inc whose-turn) (count monkeys))
                      whose-turn)]
    {:monkeys (-> monkeys
                  (update-in [whose-turn :inspected-items] inc)
                  (assoc-in [whose-turn :items] items')
                  (update-in [to-monkey :items] conj worry')
                  )
     :whose-turn whose-turn'}))

(comment
  (show-monkeys
   (:monkeys
    (monkey-throw
     (monkey-throw
      {:whose-turn 0, :monkeys (load-monkeys sample-11)}))))
  ({:id 0, :items nil, :inspected-items 2}
   {:id 1, :items (54 65 75 74), :inspected-items 0}
   {:id 2, :items (79 60 97), :inspected-items 0}
   {:id 3, :items (74 500 620), :inspected-items 0}))

(defn monkey-turn [state]
  (if (empty? (get-in state [:monkeys (:whose-turn state) :items]))
    (update state :whose-turn #(rem (inc %) (count (:monkeys state))))
    (let [{:keys [whose-turn] :as state'} (monkey-throw state)]
      (if (= whose-turn (:whose-turn state))
        (recur state')
        state'))))

(comment
  (update (monkey-turn {:whose-turn 0, :monkeys (load-monkeys sample-11)})
          :monkeys show-monkeys)
  {:monkeys ({:id 0, :items nil, :inspected-items 2}
             {:id 1, :items (54 65 75 74), :inspected-items 0}
             {:id 2, :items (79 60 97), :inspected-items 0}
             {:id 3, :items (74 500 620), :inspected-items 0}),
   :whose-turn 1})

(defn monkey-round [monkeys]
  (loop [state {:monkeys monkeys :whose-turn 0}]
    (let [state' (monkey-turn state)]
      (if (= 0 (:whose-turn state'))
        (:monkeys state')
        (recur state')))))

(comment
  (show-monkeys (monkey-round (load-monkeys sample-11)))
  ({:id 0, :items (20 23 27 26), :inspected-items 2}
   {:id 1, :items (2080 25 167 207 401 1046), :inspected-items 4}
   {:id 2, :items nil, :inspected-items 3}
   {:id 3, :items nil, :inspected-items 5})

  (show-monkeys (monkey-round (monkey-round (load-monkeys sample-11))))
  ({:id 0, :items (695 10 71 135 350), :inspected-items 6}
   {:id 1, :items (43 49 58 55 362), :inspected-items 10}
   {:id 2, :items nil, :inspected-items 4}
   {:id 3, :items nil, :inspected-items 10})

  (-> (load-monkeys sample-11)
      monkey-round
      monkey-round
      show-monkeys)
  ({:id 0, :items (695 10 71 135 350), :inspected-items 6}
   {:id 1, :items (43 49 58 55 362), :inspected-items 10}
   {:id 2, :items nil, :inspected-items 4}
   {:id 3, :items nil, :inspected-items 10}))

(defn monkey-business [monkeys]
  ;; (->> (nth (iterate monkey-round monkeys) 20) ;; <-- from Part 1
  (->> (nth (iterate monkey-round monkeys) 10000)
       (map :inspected-items)
       (sort-by -)
       (take 2)
       (apply *)))

(comment
  (monkey-business (load-monkeys sample-11))
  ;; => 10605
  (monkey-business (load-monkeys (in-lines 11)))
  ;; => 57838
  )

;; After updating for part 2
(comment
  (monkey-business (load-monkeys (in-lines 11)))
  ;; => 15050382231
  )

(defn manhattan-distance [p1 p2]
  (comment (def *dbg* {:p1 p1, :p2 p2}))
  (reduce + (map (comp abs -) p1 p2)))

(comment
  (manhattan-distance [0 0] [0 1])
  (manhattan-distance [3 -4] [-5 12]))

;; Day 10

(def sample-10-small (string/split "noop
addx 3
addx -5" #"\n"))

(def sample-10-larger (string/split "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop" #"\n"))

(def cycles {"noop" 1 "addx" 2})

(defn line->crt-instruction [line]
  (let [[op & args] (string/split line #"\s+")]
    (concat [op] (map edn/read-string args))))

(comment
  (line->crt-instruction "addx -11")
  ;; => ("addx" -11)
  (line->crt-instruction "noop")
  ;; => ("noop")
  )

(defn crt-update
  "Returns a state transition function for the CRT CPU based on the given instruction."
  [[op & args]]
  (case op
    "noop" identity
    "addx" (fn [cpu] (update cpu :X (fn [X] (+ X (first args))))))
  )

(defn crt-tick
  "Tick the cathode ray tube CPU, returning the new CPU state.
  crt cpu:
  pc is a program counter pointing at the current instruction
  noop takes one tick
  addx takes two
  eval happens *after* the instruction's delay
  one register, X. Initialized to 1.
  -eval-at is the tick at which the next eval will occur."
  [{:keys [instructions
           pc
           X
           tick
           -eval-at
           ]
    :as cpu}]
  (cond
    (nil? -eval-at) ;; No -eval-at -> initialize CPU
    (assoc cpu
           :pc 0
           :X 1
           :tick 0
           :-eval-at (cycles (first (first instructions))))

    (< tick -eval-at)
    (update cpu :tick inc)

    (< -1 pc (count instructions))
    (let [instruction (get instructions pc)
          pc' (inc pc)
          next-instruction (get instructions pc')
          next-op (first next-instruction)]
      (comment
        (def *dbg* {:instruction instruction
                    :pc' pc'
                    :next-instruction next-instruction
                    :next-op next-op
                    :cpu cpu}))
      (-> ((crt-update instruction) cpu) ;; eval CPU
          (assoc :pc pc') ;; advance program counter
          (update :-eval-at #(+ % (get cycles next-op 0))) ;; set new -eval-at deadline
          (update :tick inc)
          ))

    :else
    (update cpu :tick inc)
    ))

(comment
  (->> (take 10 (iterate crt-tick (crt-tick {:instructions [["noop"] ["addx" 3] ["addx" -5]]})))
       (map (fn [cpu] [(:tick cpu) (:-eval-at cpu) (:X cpu)]))))
;; => ([0 1 1] [1 1 1] [2 3 1] [3 3 1] [4 5 4] [5 5 4] [6 5 -1] [7 5 -1] [8 5 -1] [9 5 -1])

(defn signal-strength [cpu]
  (* (:tick cpu) (* (:X cpu))))

(defn crt-cpu-progression [instructions]
  (take-while
   (fn [{:keys [tick -eval-at]}]
     (<= tick -eval-at))
   (iterate crt-tick
            (crt-tick {:instructions instructions}))))

(comment
  (count
   (take 10 (crt-cpu-progression [["noop"] ["addx" 3] ["addx" -5]]))))
;; => 6

(defn interesting-crt-state? [{:keys [tick]}]
  (zero? (rem (- tick 20) 40)))

(defn day-10-part-1 [instructions]
  (reduce +
          (map signal-strength
               (filter interesting-crt-state?
                       (crt-cpu-progression instructions)))))

(comment
  (day-10-part-1 (mapv line->crt-instruction sample-10-larger))
  ;; => 13140

  (day-10-part-1 (mapv line->crt-instruction (in-lines 10)))
  ;; => 17180
  )

;; part 2

(def lit "#")
(def dark ".")

(defn tick->pixel-position [tick] (rem (dec tick) 40))
(comment
  (tick->pixel-position 1)
  ;; => 0
  (tick->pixel-position 40)
  ;; => 39
  (tick->pixel-position 41)
  ;; => 0
  (tick->pixel-position 80)
  ;; => 39
  )

(defn pixel [tick X]
  (if (<= (dec X)
          (tick->pixel-position tick)
          (inc X))
    lit
    dark))

(defn crt-display [instructions]
  (map
   string/join
   (partition
    40
    (into
     []
     (comp
      (map (juxt :tick :X))
      (drop 1)
      (take-while (fn [[tick X]] (<= tick 240)))
      (map (fn [[tick X]] (pixel tick X)))
      )
     (crt-cpu-progression instructions)))))

(comment
  (crt-display (mapv line->crt-instruction sample-10-larger))
  (comment
    ("##..##..##..##..##..##..##..##..##..##.."
     "###...###...###...###...###...###...###."
     "####....####....####....####....####...."
     "#####.....#####.....#####.....#####....."
     "######......######......######......####"
     "#######.......#######.......#######.....")))

(comment
  (crt-display (mapv line->crt-instruction (in-lines 10)))
  ("###..####.#..#.###..###..#....#..#.###.."
   "#..#.#....#..#.#..#.#..#.#....#..#.#..#."
   "#..#.###..####.#..#.#..#.#....#..#.###.."
   "###..#....#..#.###..###..#....#..#.#..#."
   "#.#..#....#..#.#....#.#..#....#..#.#..#."
   "#..#.####.#..#.#....#..#.####..##..###.."))

;; Day 9

(def sample-9 (string/split "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2" #"\n"))

(defn rope-abuts? [head tail]
  (let [[hr hc] head
        [tr tc] tail]
    (and (<= (abs (- hr tr)) 1)
         (<= (abs (- hc tc)) 1))))

(defn clamp [minimum maximum]
  (fn [x]
    (-> x
        (max minimum)
        (min maximum))))

(comment
  ((clamp -1 1) -2)
  ;; => -1
  ((clamp -1 1) 2)
  ;; => 1
  ((clamp -1 1) 0)
  ;; => 0
  )

(defn towards [head']
  (fn [tail]
    (if (rope-abuts? head' tail)
      tail
      ;; chase head' by up to 1 row and up to 1 column
      (let [[hr' hc'] head'
            [tr tc] tail
            dr ((clamp -1 1) (- hr' tr))
            dc ((clamp -1 1) (- hc' tc))]
        [(+ tr dr) (+ tc dc)]))))

(defn ripple-rope
  "Follow the rope after moving, segment by segment."
  [rippled rip-tail [middle & to-ripple]]
  (let [rippled' (concat rippled [rip-tail])]
    (if (empty? middle)
      rippled'
      (let [middle' ((towards rip-tail) middle)]
        (recur rippled' middle' to-ripple)))))

(comment
  (defn move-rope [direction]
    (fn [[head tail]]
      (let [head' (map + head direction)]
        [head'
         ((towards head') tail)]))))

(defn move-rope [direction]
  (fn [[head & tail]]
    (let [[hr hc] head
          head' (map + head direction)]
      (ripple-rope [] head' tail)
      )))

(def rope-origin [[0 0] [0 0]])

(comment
  ((move-rope right) rope-origin)
  ;; => {:head (0 1), :tail [0 0]}

  (nth (iterate (move-rope right) rope-origin) 4)
  ;; => {:head (0 4), :tail (0 3)}

  (nth (iterate (move-rope up) ['(0 4) '(0 3)]) 1)
  ;; (nth (iterate (move-rope up) {:head '(0 4), :tail '(0 3)}) 1)
  ;; => {:head (-1 4), :tail (0 3)}

  (nth (iterate (move-rope up) ['(0 4) '(0 3)]) 4)
  ;; (nth (iterate (move-rope up) {:head '(0 4), :tail '(0 3)}) 4)
  ;; => {:head (-4 4), :tail (-3 4)}
  )

(defn line->rope-moves [line]
  (let [[dir n-str] (string/split line #"\s+")]
    (repeat (edn/read-string n-str) (move-rope (case dir "U" up "D" down "L" left "R" right)))
    ))

(defn rope-progression [input]
  (reductions #(%2 %1) rope-origin (mapcat line->rope-moves input)))

(comment
  (rope-progression sample-9))

(defn rope-tail-visited [input]
  (into #{} (map last (rope-progression input))))

(comment
  (count (rope-tail-visited sample-9))
  ;; => 13

  (count (rope-tail-visited (in-lines 9)))
  ;; => 5930

  ;; Part 2
  (count
   (into #{}
         (map last
              (reductions #(%2 %1)
                          (repeat 10 [0 0])
                          (mapcat line->rope-moves (in-lines 9))))))
  ;; => 2443
  )

;; Day 8

(def sample-8
  (string/split
   "30373
25512
65332
33549
35390
" #"\n"))

(defn forest-from [input]
  (mapv (fn [row] (mapv edn/read-string (string/split row #""))) input))

(comment
  (forest-from sample-8)
  (forest-from (in-lines 8)))

;; Let's traverse the forest from N, S, E, and W, and for each row, count
;; the number of visible trees.

;; Okay fine, sometimes it's easier to write it out imperatively first to
;; keep one's head straight.
;;
;; max-height := -1
;; visible-trees := #{}
;; for (tree of line) {
;;   h := (tree-height tree)
;;   if (h > max-height) {
;;     max := h
;;     (conj visible-trees tree)
;;     if (max-height = 9) break;
;;   }
;; }
;; return (count visible-trees)
;;
;; So we can do a loop/recur and get an answer, even though there's probably
;; some combination of seq functions which would give us a more elegant
;; answer. Probably some transducer-y thing. Worth looking up.
;;
;; Upon further consideration, this may also simply be (reduce) vs. a seq of
;; trees with their heights. Perhaps the tricky part would be terminating the
;; reduction early once we see the first 9.

(defn tree-line [forest start-tree [delta-row delta-col]]
  (let [max-row (dec (count forest))
        max-col (dec (count (first forest)))
        in-forest? (fn [[row col]] (and (<= 0 row max-row) (<= 0 col max-col)))]
    (take-while
     in-forest?
     (iterate
      (fn [[r c]]
        [(+ r delta-row)
         (+ c delta-col)])
      start-tree))
    ))

(comment
  (tree-line sample-8 [0 0] [0 1])
  (tree-line sample-8 [0 0] [1 0])
  (tree-line sample-8 [1 1] [1 0])
  )

(defn tree-height [forest tree]
  (get-in forest tree))

(comment
  (tree-height forest [1 1]))

(defn visible-trees [forest line]
  (loop [max-height -1
         visible #{}
         [tree & remaining] line]
    (if (or (nil? tree) (<= 9 max-height))
      visible
      (let [h (tree-height forest tree)]
        (comment ;; neat technique!
          (def *dbg* {:max-height max-height
                      :visible visible
                      :tree tree
                      :remaining remaining
                      :h h
                      }))
        (recur
         (max h max-height)
         (if (< max-height h)
           (conj visible tree)
           visible)
         remaining
         )))))

(defn visible-trees-in-forest [forest]
  (let [looking-* (fn [direction] (fn [tree] (tree-line forest tree direction)))
        looking-east (looking-* [0 1])
        looking-south (looking-* [1 0])
        looking-west (looking-* [0 -1])
        looking-north (looking-* [-1 0])

        west-edge (looking-south [0 0])
        north-edge (looking-east [0 0])
        east-edge (looking-south [0 (dec (count (first forest)))])
        south-edge (looking-east [(dec (count forest)) 0])

        visible-from (fn [edge looking]
                       (apply sets/union
                              (map (fn [tree]
                                     (visible-trees forest (looking tree)))
                                   edge)))
        ]
    (sets/union (visible-from west-edge looking-east)
                (visible-from north-edge looking-south)
                (visible-from east-edge looking-west)
                (visible-from south-edge looking-north)
                )
    ))

(comment
  (count
   (visible-trees-in-forest (forest-from sample-8)))
  ;; => 21

  (identity *dbg*)

  (count
   (visible-trees-in-forest (forest-from (in-lines 8))))
  ;; => 1693
  )

(defn viewing-distance [forest tree direction]
  (let [viewer-height (tree-height forest tree)
        blocker (first (filter (fn [tree]
                                 (<= viewer-height (tree-height forest tree)))
                               (drop 1 (tree-line forest tree direction))))]
    (or (and blocker (manhattan-distance tree blocker))
        (let [[dr dc] direction
              [r c] tree]
          (if (zero? dr)
            (if (< dc 0)
              c
              (- (count (first forest)) c 1))
            (if (< dr 0)
              r
              (- (count forest) r 1))
            )))))

(comment
  (tree-line (forest-from sample-8) [1 2] [-1 0])
  ;; => ([1 2] [0 2])
  (viewing-distance (forest-from sample-8) [1 2] [-1 0])
  ;; => 1
  (viewing-distance (forest-from sample-8) [1 2] [0 -1])
  ;; => 1
  (viewing-distance (forest-from sample-8) [1 2] [0 1])
  ;; => 2
  (viewing-distance (forest-from sample-8) [1 2] [1 0])
  ;; => 2

  (viewing-distance (forest-from sample-8) [3 2] [-1 0])
  ;; => 2
  (viewing-distance (forest-from sample-8) [3 2] [0 -1])
  ;; => 2
  (viewing-distance (forest-from sample-8) [3 2] [1 0])
  ;; => 1
  (viewing-distance (forest-from sample-8) [3 2] [0 1])
  ;; => 2
  )

(defn scenic-score [forest tree]
  (* (viewing-distance forest tree [0 1]) ;; east
     (viewing-distance forest tree [1 0]) ;; south
     (viewing-distance forest tree [0 -1]) ;; west
     (viewing-distance forest tree [-1 0]) ;; north
     )
  )

(comment
  (scenic-score (forest-from sample-8) [1 2])
  ;; => 4
  (scenic-score (forest-from sample-8) [3 2])
  ;; => 8
  )

(defn scenic-scores [forest]
  (for [r (range (count forest))
        c (range (count (first forest)))]
    (scenic-score forest [r c])
    ))

(comment
  (reduce max (scenic-scores (forest-from sample-8)))

  (reduce max (scenic-scores (forest-from (in-lines 8))))
  ;; => 422059
  )

;; Day 7

(def sample-7
  ["$ cd /"
   "$ ls"
   "dir a"
   "14848514 b.txt"
   "8504156 c.dat"
   "dir d"
   "$ cd a"
   "$ ls"
   "dir e"
   "29116 f"
   "2557 g"
   "62596 h.lst"
   "$ cd e"
   "$ ls"
   "584 i"
   "$ cd .."
   "$ cd .."
   "$ cd d"
   "$ ls"
   "4060174 j"
   "8033020 d.log"
   "5626152 d.ext"
   "7214296 k"])

(def parse-log-line
  (insta/parser
   "line = cd | ls | directory | file
    ls = '$ ls'
    cd = '$ cd ' dir
    dir = '/' | '..' | #'\\w+'
    file = size #'\\s+' name
    directory = 'dir ' dir
    name = #'[A-Za-z0-9.]+'
    size = #'\\d+'"))

(comment
  (parse-log-line "$ ls")
  ;; => [:line [:ls "$ ls"]]
  (parse-log-line "$ cd xyz")
  ;; => [:line [:cd "$ cd " [:dir "xyz"]]]
  (parse-log-line "dir d")
  ;; => [:line [:directory "dir " [:dir "d"]]]

  (map parse-log-line sample-7)

  [:line [:cd "$ cd " [:dir "/"]]]
  [:line [:ls "$ ls"]]
  [:line [:directory "dir " [:dir "a"]]]
  [:line [:file [:size "14848514"] " " [:name "b.txt"]]]
  [:line [:file [:size "8504156"] " " [:name "c.dat"]]]
  [:line [:directory "dir " [:dir "d"]]]
  [:line [:cd "$ cd " [:dir "a"]]]
  [:line [:ls "$ ls"]]
  [:line [:directory "dir " [:dir "e"]]]
  [:line [:file [:size "29116"] " " [:name "f"]]]
  [:line [:file [:size "2557"] " " [:name "g"]]]
  [:line [:file [:size "62596"] " " [:name "h.lst"]]]
  [:line [:cd "$ cd " [:dir "e"]]]
  [:line [:ls "$ ls"]]
  [:line [:file [:size "584"] " " [:name "i"]]]
  [:line [:cd "$ cd " [:dir ".."]]]
  [:line [:cd "$ cd " [:dir ".."]]]
  [:line [:cd "$ cd " [:dir "d"]]]
  [:line [:ls "$ ls"]]
  [:line [:file [:size "4060174"] " " [:name "j"]]]
  [:line [:file [:size "8033020"] " " [:name "d.log"]]]
  [:line [:file [:size "5626152"] " " [:name "d.ext"]]]
  [:line [:file [:size "7214296"] " " [:name "k"]]])

(defmulti info<-line (fn [[_ [line-type]]] line-type))

(defmethod info<-line :ls [_] [:ls])
(defmethod info<-line :cd [[_ [_ _ [_ dir]]]] [:cd dir])
(defmethod info<-line :directory [[_ [_ _ dir]]] dir)
(defmethod info<-line :file [[_ [_ [_ size] _ [_ name]]]] [:file name (edn/read-string size)])

(comment
  (info<-line [:line [:ls "$ ls"]])
  ;; => [:ls]
  (info<-line [:line [:cd "$ cd " [:dir ".."]]])
  ;; => [:cd ".."]
  (info<-line [:line [:directory "dir " [:dir "a"]]])
  ;; => [:dir "a"]
  (info<-line [:line [:file [:size "4060174"] " " [:name "j"]]])
  ;; => [:file "j" 4060174]

  {"a" {"e" {"i" 584}
        "f" 29116
        "g" 2557
        "h.lst" 62596}
   "b.txt" 14848514
   "c.dat" 8504156
   "d" {"j" 4060174
        "d.log" 8033020
        "d.ext" 5626152
        "k" 7214296}}

  )

;; Y'know, maybe forget the zipper and just keep track of the file structure
;; and current directory?

(defn update-fs [[cmd & args]]
  (case cmd
    :ls
    identity

    :cd
    (let [dir (first args)]
      (fn [shell]
        (update shell
                :working-directory
                (case dir
                  ".."
                  butlast

                  "/"
                  (fn [_] ())

                  ;; else
                  #(concat % [dir]))
                )))

    :dir
    (let [dir (first args)]
      (fn [{:keys [working-directory] :as shell}]
        (update-in shell
                   (concat [:files] working-directory [dir])
                   #(or % {})
                   )))

    :file
    (let [[file size] args]
      (fn [{:keys [working-directory] :as shell}]
        (assoc-in shell
                  (concat [:files] working-directory [file])
                  size)))))


(defn infer-fs [infos]
  (let [updates (map update-fs infos)]
    (:files (reduce #(%2 %1) {:files {} :working-directory ()} updates))))


(defn files-from-input [input]
  (infer-fs (map (comp info<-line parse-log-line) input)))

(comment
  (files-from-input sample-7)
  {"a" {"e" {"i" 584},
        "f" 29116,
        "g" 2557,
        "h.lst" 62596},
   "b.txt" 14848514,
   "c.dat" 8504156,
   "d" {"j" 4060174,
        "d.log" 8033020,
        "d.ext" 5626152,
        "k" 7214296}})

(defn total-size [files path]
  (let [dir (get-in files path)]
    (cond
      (number? dir)
      dir

      (empty? dir)
      0

      :else ;; map
      (reduce + (map (fn [key] (total-size files (concat path [key]))) (keys dir)))
      ))
  )

(comment
  (total-size (files-from-input sample-7) ["a" "e"])
  (total-size (files-from-input sample-7) ["a"])
  (total-size (files-from-input sample-7) ["d"])
  (total-size (files-from-input sample-7) [])
  )

(defn all-directories [files from]
  (let [current-directory (get-in files from)]
    (when (map? current-directory)
      (concat [from] (mapcat (fn [subdir] (all-directories files (concat from [subdir]))) (keys current-directory)))
      )))

(comment
  (all-directories (files-from-input sample-7) ())
  ;; => (() ("a") ("a" "e") ("d"))
  (comment
    (let [files (files-from-input sample-7)]
      (map (fn [dir] [dir (total-size files dir)]) (all-directories files ()))))
  ;; => ([() 48381165] [("a") 94853] [("a" "e") 584] [("d") 24933642])
  )

(defn file-sizes [input]
  (let [files (files-from-input input)]
    (map (fn [dir] (total-size files dir)) (all-directories files ()))
    ))

(defn total-size-sum [input]
  (reduce + (let [files (files-from-input input)
                  sizes (map (fn [dir] (total-size files dir)) (all-directories files ()))]
              (filter #(<= % 100000) sizes))))

(comment
  (total-size-sum sample-7)
  ;; => 95437
  (total-size-sum (in-lines 7))
  ;; => 1490523
  )

;; Part 2
(defn smallest-needed [input]
  (let [total-disk 70000000
        space-needed 30000000
        files (files-from-input input)
        used (total-size files ())
        current-unused (- total-disk used)
        required (- space-needed current-unused)]
    (apply min (filter #(<= required %) (file-sizes input)))
    ))

(comment
  (smallest-needed sample-7)
  ;; => 24933642

  (smallest-needed (in-lines 7))
  ;; => 12390492
  )

;; Day 6

(defn packet-start
  ([input] (packet-start 4 (take 4 input) (drop 4 input)))
  ([index window remainder]
   (if (= 4 (count (set window)))
     index
     (let [[head & tail] remainder]
       (recur (inc index) (concat (drop 1 window) [head]) tail)))))

;; (packet-start "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
;; => 7

;; (packet-start (first (in-lines 6)))
;; => 1896

(defn message-start
  ([input] (message-start 14 (take 14 input) (drop 14 input)))
  ([index window remainder]
   (if (= 14 (count (set window)))
     index
     (let [[head & tail] remainder]
       (recur (inc index) (concat (drop 1 window) [head]) tail)))))

(comment
  (map message-start [
                      "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
                      "bvwbjplbgvbhsrlpgdmjqwftvncz"
                      "nppdvjthqldpwncqszvftbrmjlhg"
                      "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
                      "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
                      ]))
;; => (19 23 23 29 26)

;; (message-start (first (in-lines 6)))
;; => 3452

;; Day 5

(defn stack [s]
  (reduce conj () (reverse (map str s))))

(comment
  {"1" (stack "QGPRLCTF")
   "2" (stack "JSFRWHQN")
   "3" (stack "QMPWHBF")
   "4" (stack "FDTSV")
   "5" (stack "ZFVWDLQ")
   "6" (stack "SLCZ")
   "7" (stack "FDVMBZ")
   "8" (stack "BJT")
   "9" (stack "HPSLGBNQ")}

  (first (drop 10 (in-lines 5))))
;; => "move 1 from 8 to 1"

(defn stack-step [s]
  (let [[_ rep-str from to]
        (re-matches #"move (\d+) from (\d+) to (\d+)" s)]
    (repeat (edn/read-string rep-str) (fn [stacks]
                                        (let [crate (first (stacks from))]
                                          (-> stacks
                                              (update from rest)
                                              (update to conj crate)))))))

(comment
  (let [stacks {"1" (stack "NZ")
                "2" (stack "DCM")
                "3" (stack "P")}
        steps ["move 1 from 2 to 1"]]
    (reduce #(%2 %1) stacks (mapcat stack-step steps)))
  ;; => {"1" ("D" "N" "Z"), "2" ("C" "M"), "3" ("P")}

  (let [stacks {"1" (stack "NZ")
                "2" (stack "DCM")
                "3" (stack "P")}
        steps ["move 1 from 2 to 1"
               "move 3 from 1 to 3"]]
    (reduce #(%2 %1) stacks (mapcat stack-step steps)))
  ;; => {"1" (), "2" ("C" "M"), "3" ("Z" "N" "D" "P")}

  (let [stacks {"1" (stack "NZ")
                "2" (stack "DCM")
                "3" (stack "P")}
        steps ["move 1 from 2 to 1"
               "move 3 from 1 to 3"
               "move 2 from 2 to 1"]]
    (reduce #(%2 %1) stacks (mapcat stack-step steps)))
  ;; => {"1" ("M" "C"), "2" (), "3" ("Z" "N" "D" "P")}

  (let [stacks {"1" (stack "NZ")
                "2" (stack "DCM")
                "3" (stack "P")}
        steps ["move 1 from 2 to 1"
               "move 3 from 1 to 3"
               "move 2 from 2 to 1"
               "move 1 from 1 to 2"]]
    (reduce #(%2 %1) stacks (mapcat stack-step steps))))
;; => {"1" ("C"), "2" ("M"), "3" ("Z" "N" "D" "P")}

(defn move-crates [stacks procedure]
  (reduce #(%2 %1) stacks (mapcat stack-step procedure)))

(comment
  (move-crates
   {"1" (stack "NZ")
    "2" (stack "DCM")
    "3" (stack "P")}
   ["move 1 from 2 to 1"
    "move 3 from 1 to 3"
    "move 2 from 2 to 1"
    "move 1 from 1 to 2"])
  ;; => {"1" ("C"), "2" ("M"), "3" ("Z" "N" "D" "P")}

  (let [final-crates
        (move-crates
         {"1" (stack "QGPRLCTF")
          "2" (stack "JSFRWHQN")
          "3" (stack "QMPWHBF")
          "4" (stack "FDTSV")
          "5" (stack "ZFVWDLQ")
          "6" (stack "SLCZ")
          "7" (stack "FDVMBZ")
          "8" (stack "BJT")
          "9" (stack "HPSLGBNQ")}
         (drop 10 (in-lines 5)))]
    (apply str (map (comp first final-crates str inc) (range 9)))))
;; => "VGBBJCRMN"

(defn stack-9001 [s]
  (into [] (map str) s))

;; (stack-9001 "abc")
;; => ["a" "b" "c"]

(defn stack-step-9001 [s]
  (let [[_ rep-str from to]
        (re-matches #"move (\d+) from (\d+) to (\d+)" s)
        crates (edn/read-string rep-str)]
    (fn [stacks]
      (let [substack (take crates (stacks from))]
        (-> stacks
            (update from #(drop crates %))
            (update to #(concat substack %)))))))

(defn move-crates-9001 [stacks procedure]
  (reduce #(%2 %1) stacks (map stack-step-9001 procedure)))

(comment
  (let [final-crates
        (move-crates-9001
         {"1" (stack "QGPRLCTF")
          "2" (stack "JSFRWHQN")
          "3" (stack "QMPWHBF")
          "4" (stack "FDTSV")
          "5" (stack "ZFVWDLQ")
          "6" (stack "SLCZ")
          "7" (stack "FDVMBZ")
          "8" (stack "BJT")
          "9" (stack "HPSLGBNQ")}
         (drop 10 (in-lines 5)))]
    (apply str (map (comp first final-crates str inc) (range 9)))))
;; => "LBBVJBRMH"

;; Day 4

(defn assignment [s]
  (map edn/read-string (string/split s #"-")))

;; (assignment "2-4")
;; => (2 4)

(defn assignment-pair [s]
  (map assignment (string/split s #",")))

;; (assignment-pair "2-4,6-8")
;; => ((2 4) (6 8))

(defn fully-overlaps? [left right]
  (let [[l-bottom l-top] left
        [r-bottom r-top] right]
    (cond
      (= l-bottom r-bottom) true
      (< r-bottom l-bottom) (recur right left)
      :else (<= r-top l-top))))

;; (fully-overlaps? [0 5] [1 3])
;; => true
;; (fully-overlaps? [0 3] [1 5])
;; => false

;; (count (filter #(apply fully-overlaps? %) (map assignment-pair (in-lines 4))))
;; => 571

(defn overlaps? [left right]
  (let [[l-bottom l-top] left
        [r-bottom r-top] right]
    (cond
      (= l-bottom r-bottom) true
      (< r-bottom l-bottom) (recur right left)
      :else (<= r-bottom l-top))))

;; (overlaps? [5 7] [7 9])
;; => true
;; (overlaps? [5 7] [8 9])
;; => false

;; (count (filter #(apply overlaps? %) (map assignment-pair (in-lines 4))))
;; => 917

;; Day 3

;; Find the item type that appears in both compartments of each rucksack.
;; What is the sum of the priorities of those item types?

(defn both-compartments-item [rucksack]
  (let [[left right] (partition (/ (count rucksack) 2) rucksack)]
    (first (sets/intersection (set left) (set right)))))
;; => #'advent.y2022/both-compartments-item

;; (both-compartments-item "vJrwpWtwJgWrhcsFMMfFFhFp")
;; => \p

(defn item-priority [item]
  (let [A (int \A)
        Z (int \Z)
        a (int \a)
        z (int \z)
        i (int item)]
    (cond
      (<= a i z)
      (inc (- (int item) (int \a)))

      (<= A i Z)
      (+ 27 (- (int item) (int \A)))
      )))

(comment
  (item-priority \a)
  ;; => 1
  (item-priority \z)
  ;; => 26
  (item-priority \A)
  ;; => 27

  (reduce + (map (comp item-priority both-compartments-item) (in-lines 3))))
;; => 7793

(defn badge-item [rucksacks]
  (first (apply sets/intersection (map set rucksacks))))

(comment
  (badge-item
   ["vJrwpWtwJgWrhcsFMMfFFhFp"
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
    "PmmdzqPrVvPwwTWBwg"])
  ;; => \r

  (reduce + (map (comp item-priority badge-item) (partition 3 (in-lines 3)))))
;; => 2499

;; Day 2

(defn rps-result [them you]
  (get-in
   {:Rock {:Rock :tie
           :Paper :win
           :Scissors :loss}
    :Paper {:Rock :loss
            :Paper :tie
            :Scissors :win}
    :Scissors {:Rock :win
               :Paper :loss
               :Scissors :tie}}
   [them you]))

(comment
  (rps-result :Rock :Scissors)
  ;; => :loss
  (rps-result :Scissors :Rock))
;; => :win

(defn rps-score [them you]
  (let [result (rps-result them you)]
    (+
     ({:loss 0
       :tie 3
       :win 6}
      result)
     ({:Rock 1
       :Paper 2
       :Scissors 3}
      you))))

(comment
  (rps-score :Rock :Paper)
  ;; => 8
  (rps-score :Paper :Rock)
  ;; => 1
  (rps-score :Scissors :Scissors))
;; => 6

(defn rps-decode [key moves]
  (let [[abc xyz] moves]
    (rps-score (key abc) (key xyz))))

(defn rps-strategy-score [key rounds]
  (reduce + (map #(rps-decode key %) rounds)))

(comment
  (let [key {"A" :Rock
             "B" :Paper
             "C" :Scissors
             "X" :Rock
             "Y" :Paper
             "Z" :Scissors}]
    (map #(rps-decode key %) (map #(string/split % #"\s+") (in-lines 2))))

  (rps-strategy-score
   {"A" :Rock
    "B" :Paper
    "C" :Scissors
    "X" :Rock
    "Y" :Paper
    "Z" :Scissors}
   [["A" "Y"]
    ["B" "X"]
    ["C" "Z"]]))

(comment
  (rps-strategy-score
   {"A" :Rock
    "B" :Paper
    "C" :Scissors
    "X" :Rock
    "Y" :Paper
    "Z" :Scissors}
   (map #(string/split % #"\s+") (in-lines 2)))
  ;; => 11603

  (take 3 (map #(string/split % #"\s+") (in-lines 2)))
  ;; => (["B" "Y"] ["A" "Z"] ["A" "Z"])
  (last (map #(string/split % #"\s+") (in-lines 2)))
  ;; => ["A" "Z"]
  (first (map #(string/split % #"\s+") (in-lines 2))))
;; => ["B" "Y"]

(defn rps-2-score [abc xyz]
  (get-in {
           ;; vs Rock
           "A" {"X" (+ 0 3)
                "Y" (+ 3 1)
                "Z" (+ 6 2)}
           ;; vs Paper
           "B" {"X" (+ 0 1)
                "Y" (+ 3 2)
                "Z" (+ 6 3)}
           ;; vs Scissors
           "C" {"X" (+ 0 2)
                "Y" (+ 3 3)
                "Z" (+ 6 1)}
           } [abc xyz]))

(comment
  (reduce + (map #(apply rps-2-score %) (map #(string/split % #"\s+") (in-lines 2)))))
;; => 12725

;; Day 1
(comment

  (apply max (filter some? (map #(apply + %) (partition-by some? (in 1)))))
  ;; => 71502

  (apply + (take 3 (reverse (sort (filter some? (map #(apply + %) (partition-by some? (in 1)))))))))
;; => 208191
