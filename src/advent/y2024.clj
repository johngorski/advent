(ns advent.y2024
  (:require
   [advent.grid :as grids]
   [advent.puzzle :as puzzle]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as sets]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [instaparse.core :as insta]
   [loom.alg :as graph-algs]
   [loom.graph :as loom]))


;; Day 6

(defn lab-from-lines [lines]
  (let [lab-grid (grids/grid lines)]
    {:in-lab? (grids/bounds-checker lab-grid)
     :obstruction-locs (into #{} (grids/val-grid-locations "#" lab-grid))
     :guard {:position (first (grids/val-grid-locations "^" lab-grid))
             :direction [-1 0]}})) ;; north

(defn turn-right [direction]
  (case direction
    [-1  0] [ 0  1]  ;; north to east
    [ 0  1] [ 1  0]  ;; east to south
    [ 1  0] [ 0 -1]  ;; south to west
    [ 0 -1] [-1  0]));; west to north

(defn turn-guard-right [guard]
  (update guard :direction turn-right))

(defn next-guard-loc [{:keys [direction position] :as guard}]
  (grids/v+ position direction))

(defn move-guard-forward [guard]
  (assoc guard :position (next-guard-loc guard)))

(defn guard-mover [obstruction-locs]
  (fn [guard]
    (if (obstruction-locs (next-guard-loc guard))
      (turn-guard-right guard)
      (move-guard-forward guard))))


(defn guard-path [{:keys [in-lab?
                          obstruction-locs
                          guard]
                   :as lab}]
  (let [move-guard (guard-mover obstruction-locs)]
    (sequence
     (comp
      (map :position)
      (take-while in-lab?))
     (iterate move-guard guard))))

(defn solve-day-6-part-1 [lines]
  (count (into #{} (guard-path (lab-from-lines lines)))))

;; Day 6 part 2 approach:
;; Proposed: Since only the guard moves, anything causing the guard to repeat a state will cause a loop.
;; - if the next square forward is unvisited and not an obstacle (or the starting point, or out of bounds),
;;   - check if placing an obstacle there would return a guard to a previous state.

;; Aaaah, hold up: this only works for cases where the guard immediately turns on to the path.
;; It misses the case where the guard needs to advance forward in order to rendezvous with a previous state.
;; And it misses the case where you step forward and then ricochet on to an obstacle that leads to a state
;; repetition.

;; So really, best approach is probaly brute-force. But not after midnight.
;; Let's get a simple cycle-detection routine together, then brute-force it step by step in the state.
;; But not after midnight. Sleep now.

(defn guard-states [{:keys [in-lab?
                            obstruction-locs
                            guard]
                     :as lab}]
  (let [move-guard (guard-mover obstruction-locs)]
    (sequence
     (take-while (comp in-lab? :position))
     (iterate move-guard guard))))

#_(defn looping-obstructions [{:keys [in-lab?
                                    obstruction-locs
                                    guard]
                             :as lab}]
  (let [move-guard (guard-mover obstruction-locs)]
    (loop [{:keys [obstruction-acc
                   visited
                   guard-history
                   guard]
            :as props}
           {:obstruction-acc #{}
            :visited #{(:position guard)}
            :guard-history #{}
            :guard guard}]))
  (sequence
   (take-while (comp in-lab? :position))
   (iterate move-guard guard)))


(defn solve-day-6-part-2 [])


;; Day 5

(defn parse-rule [line]
  (string/split line #"\|"))

(defn parse-update [line]
  (string/split line #","))

(defn rule-map [line-rules]
  (reduce
   (fn [rule-map [before after]]
     (update rule-map before #(conj (or % #{}) after)))
   {}
   line-rules))

(defn parse-ordering-rules [lines]
  (let [[rule-lines _ update-lines] (partition-by empty? lines)
        rules (map parse-rule rule-lines)]
    {:rules rules
     :rule-map (rule-map rules)
     :updates (map parse-update update-lines)}))

(defn partial-order-checker
  "From the given rules, returns a function to tell whether a precedes b."
  [rules]
  (fn [[a b]]
    ;; Attempt: only falsified if a is supposed to follow b
    (not ((get rules b #{}) a))))

(defn sequence-pairs [[before & tail]]
  (if before
    (lazy-seq (concat (map (fn [after]
                             [before after])
                           tail)
                      (sequence-pairs tail)))
    ()))

(defn update-follows-order [update-checker manual-update]
  (every? update-checker (sequence-pairs manual-update)))

(defn middle-update-element [upd]
  (nth upd (quot (dec (count upd))
                 2)))

(defn solve-day-5-part-1 [lines]
  (let [{:keys [rules rule-map updates]} (parse-ordering-rules lines)
        checker (partial-order-checker rule-map)]
    (reduce +
            (sequence
             (comp
              (filter (fn [upd]
                        (update-follows-order checker upd)))
              (map middle-update-element)
              (map edn/read-string))
             updates))))

(defn bf-ordinals [graph-map]
  (into {}
        (map-indexed (fn [idx v]
                       [v idx]))
        (graph-algs/topsort ;; graph-algs/bf-traverse
         (loom/digraph graph-map))))

(comment
 (bf-ordinals {1 #{2 3} 2 #{3 4}})
 {1 0, 3 1, 2 2, 4 3})

(comment
  (defn bf-order
    "Takes the graph represented by g-adjacency-map (source vertices to sets of destination vertices) and returns a map from vertex to BFS order."
    [rule-map upd]
    (let [ordinals (bf-ordinals rule-map)]))

  (loom/digraph {1 #{2 3} 2 #{3 1}})
  #loom.graph.BasicEditableDigraph{:nodeset #{1 3 2},
                                   :adj {1 #{3 2}, 2 #{1 3}},
                                   :in {3 #{1 2}, 2 #{1}, 1 #{2}}}

  (loom/edges #loom.graph.BasicEditableDigraph{:nodeset #{1 3 2},
                                               :adj {1 #{3 2}, 2 #{1 3}},
                                               :in {3 #{1 2}, 2 #{1}, 1 #{2}}})

  (graph-algs/bf-traverse (loom/digraph {1 #{2 3} 2 #{3 1} 3 #{4}}))
  ;; => [1 3 2 4]
  ;; => [1 3 2]
  ())

(defn compare-preserving-nil [a b]
  (if (some nil? [a b])
    0
    (compare a b)))


(defn partial-ordering-sorter
  "Reterns fn that sorts an upd according to the partial ordering checked by checker."
  ;; Actually, maybe what we want is the rule map.
  ;; Idea: BFS for the rule map. Make this a static order.
  ;; Sort each list by its BFS index and it will be sorted by partial order, too.
  ;; Main question: What's the earliest in the partial order? What if there are multiple?
  ;; GraphViz sounds helpful for inspiration.
  ;; Could maybe start somewhere arbitrary and then add to get other indices?
  ;; oh wait wait wait, piece o' cake: all nodes without predecessors are in the initial BFS queue.
  ;; So the question: Write (another) BFS, find a Clojure graph protocol to adapt for BFS, or
  ;; write a Clojure graph protocol for my future use and others' good times.
  [rule-map]
  (let [ordinals (bf-ordinals rule-map)
        keyfn ordinals]
    (fn [upd]
      (sort-by ordinals compare-preserving-nil upd))))

(defn solve-day-5-part-2 [lines]
  (let [{:keys [rules rule-map updates]} (parse-ordering-rules lines)
        checker (partial-order-checker rule-map)]
    (reduce +
            (sequence
             (comp
              (remove (fn [upd]
                        (update-follows-order checker upd)))
              (map (partial-ordering-sorter rule-map))
              (map middle-update-element)
              (map edn/read-string))
             updates))))


;; Day 4
;; Okay, after this I'm *DONE* reimplementing grid functions. Last time. Then over
;; into the advent.grid ns.

(defn grid [lines]
  (mapv #(string/split % #"") lines))

(defn grid-size [grid]
  {:rows (count grid)
   :columns (count (first grid))})

(defn bounds-checker [grid]
  (let [{:keys [rows columns]} (grid-size grid)]
    (fn [[row-idx col-idx]]
      (and (<= 0 row-idx (dec rows))
           (<= 0 col-idx (dec columns))))))

(def v+ (partial mapv +))

(defn cell-at [grid location]
  (let [[r c] location]
    (get-in grid [r c])))

(defn loc-seq
  [grid start direction]
  (let [in-bounds? (bounds-checker grid)
        step (fn [loc] (v+ loc direction))]
    (sequence
     (take-while in-bounds?)
     (iterate step start))))

(defn cell-seq
  "Starting from location start in grid, move in direction increments until running off
  the end of the grid."
  [grid start direction]
  (map (fn [loc]
         (cell-at grid loc))
       (loc-seq grid start direction)))

(defn cell-locs
  [grid]
  (let [{:keys [rows columns]} (grid-size grid)]
    (for [r (range rows)
          c (range columns)]
      [r c])))

;; end grid functions. Move them soon!

(def crossword-directions
  (for [dr [-1 0 1]
        dc [-1 0 1]
        :when (not (and (zero? dr) (zero? dc)))]
    [dr dc]))

(defn val-grid-locations [val grid]
  (filter (fn [loc]
            (= val (cell-at grid loc)))
          (cell-locs grid)))

(defn x-locations [grid]
  (val-grid-locations "X" grid))

(defn xmas-at?
  "Whether 'XMAS' is in grid starting at loc and going in dir"
  [grid loc dir]
  (= "XMAS" (apply str (take 4 (cell-seq grid loc dir)))))

(defn xmases-at
  "Directions in which XMAS is in grid starting at loc"
  [grid loc]
  (sequence
   (filter (fn [dir]
             (xmas-at? grid loc dir)))
   crossword-directions))

(defn solve-day-4-part-1 [lines]
  (let [g (grid lines)]
    (->> (x-locations g)
         (map (fn [loc]
                (count (xmases-at g loc))))
         (reduce +))))

(defn a-locations [grid]
  (val-grid-locations "A" grid))

(defn x-mas-at? [grid a-loc]
  (let [se (cell-seq grid (v+ a-loc [-1 -1]) [1 1])
        ne (cell-seq grid (v+ a-loc [-1 1]) [1 -1])
        is-mas? (fn [s-seq]
                  (#{"MAS" "SAM"}
                   (apply str (take 3 s-seq))))]
    (and (is-mas? se) (is-mas? ne))))

(defn solve-day-4-part-2 [lines]
  (let [g (grid lines)]
    (->> (a-locations g)
         (filter (fn [loc]
                   (x-mas-at? g loc)))
         count)))

;; Day 3

(def mul-re #"mul\(\d+,\d+\)")

(defn get-muls [s]
  (re-seq mul-re s))

(defn extract-mul [s]
  (when-let [match (re-matches #"mul\((\d+),(\d+)\)" s)]
    (let [[_ a-str b-str] match
          a (edn/read-string a-str)
          b (edn/read-string b-str)]
      [:mul a b])))

(defn mul [a b]
  (* a b))

(defn mull [[_ a b]]
  (mul a b))

(defn solve-day-3-part-1 [in]
  (reduce +
          (sequence
           (comp
            (map extract-mul)
            (map mull))
           (get-muls in))))

(def do-re #"do\(\)")
(def don't-re #"don't\(\)")

(defn get-ops [in]
  (map first (re-seq #"(mul\(\d+,\d+\)|do\(\)|don't\(\))" in)))

(defn extract-do [s]
  (when (re-matches do-re s) [:do]))

(defn extract-don't [s]
  (when (re-matches don't-re s) [:don't]))

(defn extract-op [s]
  (or (extract-mul s)
      (extract-do s)
      (extract-don't s)))

(defn parse-ops [in]
  (map extract-op (get-ops in)))

(defn reduce-mul [cpu [_ a b]]
  (if ((:enabled-ops cpu) :mul)
    (update cpu
            :accumulator
            #(let [product (mul a b)]
               (+ (or % 0) product)))
    cpu))

(defn reduce-do [cpu [_ a b]]
  (update cpu :enabled-ops conj :mul))

(defn reduce-don't [cpu [_ a b]]
  (update cpu :enabled-ops disj :mul))

(defn op-reducer [cpu instr]
  (case (first instr)
    :mul (reduce-mul cpu instr)
    :do (reduce-do cpu instr)
    :don't (reduce-don't cpu instr)))

(defn apply-ops [cpu ops]
  (reduce
   op-reducer
   cpu
   ops))

(defn solve-day-3-part-2 [in]
  (:accumulator
   (apply-ops
    {:accumulator 0
     :enabled-ops #{:mul}}
    (parse-ops in))))


;; Day 2

(defn parse-report [line]
  (mapv edn/read-string (string/split line #"\s+")))

(defn all-increasing? [report]
  (apply < report))

(defn all-decreasing? [report]
  (apply > report))

(defn level-diffs [report]
  (map #(apply - %) (partition 2 1 report)))

(defn safe-diffs? [report]
  (every? #(<= 1 % 3) (map abs (level-diffs report))))

(defn safe-report? [report]
  (and (or (all-increasing? report)
           (all-decreasing? report))
       (safe-diffs? report)))

(defn solve-day-2-part-1 [lines]
  (reduce + (sequence
             (comp
              (map parse-report)
              (map #(if (safe-report? %) 1 0)))
             lines)))

(defn dampened-reports [report]
  (let [len (count report)]
    (map (fn [idx]
           (let [
                 front (subvec report 0 idx)
                 back (subvec report (inc idx) len)]
             (concat front back)))
         (range len))))

(defn safe-under-dampening? [report]
  (some safe-report? (dampened-reports report)))

(defn solve-day-2-part-2 [lines]
  (reduce + (sequence
             (comp
              (map parse-report)
              (map #(if (safe-under-dampening? %) 1 0)))
             lines)))

;; Day 1

(defn line-pair [line]
  (map edn/read-string (string/split line #"\s+")))


(defn parse-day-1-lines [lines]
  (let [parsed (map line-pair lines)]
    [(sort (map first parsed)) (sort (map second parsed))]))


(defn distance [x y]
  (abs (- x y)))


(defn solve-day-1-pt-1 [lines]
  (let [[left right] (parse-day-1-lines lines)]
    (reduce + (map distance left right))))


(defn solve-day-1-pt-2 [lines]
  (let [[left right] (parse-day-1-lines lines)
        right-counts (frequencies right)
        sim-score (fn [x]
                    (* x (get right-counts x 0)))]
    (reduce + (map sim-score left))))
