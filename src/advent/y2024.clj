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


;; Day 10

(defn topo-map [lines]
  (grids/mapg parse-long (grids/from-lines lines)))

(defn trailheads [topo]
  (grids/val-grid-locations 0 topo))

(defn trails-at [topo trailhead-loc]
  (loop [climb-to 1
         trails [(list trailhead-loc)]]
    (if (< 9 climb-to)
      trails
      (recur (inc climb-to)
             (sequence
              (comp
               (mapcat (fn [trail]
                         (map (fn [neighbor]
                                (conj trail neighbor))
                              (grids/orthogonal-neighbor-locs topo (first trail)))))
               (filter (fn [trail]
                         (= climb-to (grids/cell-at topo (first trail))))))
              trails)))))


(defn trailhead-score [topo trailhead-loc]
  (count
   (into #{}
         (map first)
         (trails-at topo trailhead-loc))))


(defn solve-day-10-part-1 [lines]
  (let [topo (topo-map lines)]
    (reduce +
            (map (fn [trailhead]
                   (trailhead-score topo trailhead))
                 (trailheads topo)))))

(defn trailhead-rating [topo trailhead-loc]
  (count (trails-at topo trailhead-loc)))

(defn solve-day-10-part-2 [lines]
  (let [topo (topo-map lines)]
    (reduce +
            (map (fn [trailhead]
                   (trailhead-rating topo trailhead))
                 (trailheads topo)))))


;; Day 9

(defn disk-seq
  "Very easy to second-guess a representation here. But whatever that representation is,
  here is where we parse it from the input."
  [in]
  (->> (map parse-long
            (string/split in #""))
       (partition 2 2 [0])
       (map-indexed (fn [file-id [file-len free-len]]
                      (concat (repeat file-len file-id)
                              (repeat (or free-len 0) nil))))
       (apply concat)
       (into [])))

(defn compact-files
  ([disk] (compact-files 0 disk))
  ([free-idx disk]
   (let [tail-idx (dec (count disk))]
     (cond
       (<= 0 tail-idx free-idx)
       disk

       (disk free-idx)
       (recur (inc free-idx) disk)

       (nil? (disk tail-idx))
       (recur free-idx (subvec disk 0 tail-idx))

       :else
       (recur (inc free-idx) (-> disk
                                 (assoc free-idx (disk tail-idx))
                                 (subvec 0 tail-idx)
                                 ))))))

(defn filesystem-checksum [compacted-file-seq]
  (->> compacted-file-seq
       (map-indexed (fn [idx file-id]
                      (* idx (or file-id 0))))
       (reduce +)))

(defn solve-day-9-part-1 [in]
  ((comp filesystem-checksum compact-files disk-seq) in))

(defn free-space-at [disk free-idx]
  (loop [acc 0
         idx free-idx]
    (if (disk idx)
      acc
      (recur (inc acc) (inc idx)))))

(defn file-size [disk file-idx]
  (if-let [file-id (disk file-idx)]
    (loop [acc 1
           idx (dec file-idx)]
      (cond
        (< idx 0)
        acc

        (= file-id (disk idx))
        (recur (inc acc) (dec idx))

        :else
        acc))
    0))

(defn free-blocks
  "Returns left-to-right vec of free blocks with :size and :start-idx keys."
  [disk]
  (let [disk-len (count disk)]
    (loop [acc []
           idx 0
           started-free-block nil]
      (cond
        (<= disk-len idx)
        acc

        (disk idx)
        (if started-free-block
          (recur (conj acc started-free-block) (inc idx) nil)
          (recur acc (inc idx) nil))

        :else ;; free block
        (if started-free-block
          (recur acc (inc idx) (update started-free-block :size inc))
          (recur acc (inc idx) {:size 1 :start-idx idx}))))))

(defn move-file-on-disk
  [disk from length to]
  (if (<= length 0)
    disk
    (recur (assoc disk to (disk from), from nil)
           (dec from)
           (dec length)
           (inc to))))

(defn dissoc-seq
  "Return a (hopefully lazy seq) from seq which skips over the item at the idx-th place."
  [seq idx]
  (let [before (take idx seq)
        after (drop (inc idx) seq)]
    (concat before after)))

(defn update-free-blocks-from-move
  "Very specific to the AoC problem. Only reduces free blocks, because that's all day 9 part 2 requires.
  block-idx is the index of the free block, not the index of the disk.
  Freaks out if length > block size because that helps us solve the problem sooner."
  [blocks block-idx length]
  (let [block (blocks block-idx)
        size' (- (block :size) length)]
    (cond
      (< size' 0)
      (throw (ex-info "Block size insufficient for move."
                      {:size (block :size), :length length, :block-idx block-idx, :blocks blocks}))

      (= 0 size')
      (vec (dissoc-seq blocks block-idx))

      :else
      (let [start-idx' (+ length (block :start-idx))]
        (assoc blocks block-idx {:size size' :start-idx start-idx'})))))

(defn block-index-of-min-size [blocks size-needed]
  (let [num-blocks (count blocks)]
    (loop [block-idx 0]
      (when (< block-idx num-blocks)
        (if (<= size-needed (get-in blocks [block-idx :size]))
          block-idx
          (recur (inc block-idx)))))))


;; TODO: Find the bug.
(defn compact-files-no-frag
  ;; Find block index by target index
  ;; Update block. Remove empty free blocks.
  ([disk] (compact-files-no-frag disk (free-blocks disk) (dec (count disk))))
  ([disk free-blocks tail-idx]
   (if (< tail-idx 0)
     ;; tail pointer has reached the front of the disk
     (do
       (def *dbg*
         {:disk disk
          :free-blocks free-blocks
          :tail-idx tail-idx})
       disk)

     (let [move-length (file-size disk tail-idx)]
       (if (<= move-length 0) ;; tail pointer is inside of a free block
         (recur disk free-blocks (dec tail-idx))

         ;; handle the full file move at once; fit or no fit, next iteration is past the current file.
         (let [tail-idx' (- tail-idx move-length)]
           (if-let [block-idx (block-index-of-min-size free-blocks move-length)]
             ;; we have a block of sufficient size
             (let [block (free-blocks block-idx)
                   block-start (block :start-idx)
                   disk' (move-file-on-disk disk tail-idx move-length block-start)
                   free-blocks' (update-free-blocks-from-move free-blocks block-idx move-length)]
               (recur disk' free-blocks' tail-idx'))

             (recur disk free-blocks tail-idx'))))))))


;; Day 8

(defn frequency-locations [grid]
  (-> (grids/val-location-map grid)
      (dissoc ".")))

(defn pair-antinodes [loc-pair]
  (let [[a b] loc-pair
        a-to-b (grids/v- b a)]
    [(grids/v+ b a-to-b)
     (grids/v- a a-to-b)]))

(defn all-pairs [v]
  (let [len (count v)]
    (for [a (range len)
          b (range (inc a) len)]
      (mapv v [a b]))))

(defn antenna-freq-antinodes [antenna-freq-locs]
  (into #{}
        (mapcat (fn [loc-pair] (pair-antinodes loc-pair)))
        (all-pairs antenna-freq-locs)))

(defn grid-antinodes [grid]
  (let [in-bounds? (grids/bounds-checker grid)]
    (into #{}
          (comp
           (mapcat antenna-freq-antinodes)
           (filter in-bounds?))
          (vals (frequency-locations grid)))))

(defn solve-day-8-part-1 [lines]
  (count (grid-antinodes (grids/from-lines lines))))

(defn pair-harmonic-antinodes [grid loc-pair]
  (let [[a b] loc-pair
        dir (grids/v- b a)]
    (into #{}
          (concat (grids/loc-seq grid a dir)
                  (grids/loc-seq grid a (grids/v- dir))))))

(defn antenna-freq-harmonic-antinodes [grid antenna-freq-locs]
  (into #{}
        (mapcat (fn [loc-pair] (pair-harmonic-antinodes grid loc-pair)))
        (all-pairs antenna-freq-locs)))

(defn grid-harmonic-antinodes [grid]
  (into #{}
        (mapcat (fn [antenna-freq-locs] (antenna-freq-harmonic-antinodes grid antenna-freq-locs)))
        (vals (frequency-locations grid))))

(defn solve-day-8-part-2 [lines]
  (count (grid-harmonic-antinodes (grids/from-lines lines))))


;; Day 7
;; The nice thing about putting new days at the top is that without forward declarations, tantalizing reuse immediately suggests
;; movement into another ns.

(defn bridge-calibration-input [line]
  (let [[test-value-s factors-s] (string/split line #"\s*:\s*")
        factor-ss (string/split factors-s #"\s+")]
    {:test-value (edn/read-string test-value-s)
     :factors (map edn/read-string factor-ss)}))

(defn bridge-calibration-op-seqs
  ([factor-count]
   (bridge-calibration-op-seqs factor-count [[]]))
  ([factor-count op-seqs-acc]
   (if (< factor-count 1)
     op-seqs-acc
     (recur (dec factor-count)
            (for [op-seq op-seqs-acc
                  last-op [+ *]]
              (conj op-seq last-op))))))

(defn apply-op-seq [[start & factors] ops]
  (loop [acc start
         remaining-factors factors
         remaining-ops ops]
    (if (empty? remaining-factors)
      acc
      (let [[factor & factors'] remaining-factors
            [op & ops'] remaining-ops]
        (recur (op acc factor) factors' ops')))))

(defn calibration-satisfaction-predicate [{:keys [test-value factors]}]
  (fn [ops]
    (= test-value (apply-op-seq factors ops))))

(defn bridge-calibration-satisfyable? [{:keys [test-value factors] :as calibration}]
  (let [satisfies? (calibration-satisfaction-predicate calibration)]
    (some satisfies? (bridge-calibration-op-seqs (count factors)))))

(defn solve-day-7-part-1 [lines]
  (reduce +
          (sequence
           (comp
            (map bridge-calibration-input)
            (filter bridge-calibration-satisfyable?)
            (map :test-value))
           lines)))

(defn ||
  "\"combines the digits from its left and right inputs into a single number. For example, 12 || 345 would become 12345.\""
  [a b]
  (edn/read-string (str a b)))

(defn bridge-calibration-op-seqs-with-concat
  ([factor-count]
   (bridge-calibration-op-seqs-with-concat factor-count [[]]))
  ([factor-count op-seqs-acc]
   (if (< factor-count 1)
     op-seqs-acc
     (recur (dec factor-count)
            (for [op-seq op-seqs-acc
                  last-op [+ * ||]]
              (conj op-seq last-op))))))

(defn bridge-calibration-satisfyable-with-concat? [{:keys [test-value factors] :as calibration}]
  (let [satisfies? (calibration-satisfaction-predicate calibration)]
    (some satisfies? (bridge-calibration-op-seqs-with-concat (count factors)))))

(defn solve-day-7-part-2 [lines]
  (reduce +
          (sequence
           (comp
            (map bridge-calibration-input)
            (filter bridge-calibration-satisfyable-with-concat?)
            (map :test-value))
           lines)))


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
    [ 0 -1] [-1  0]))

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

;; 1. put hypothetical boulder in front (if path not already traversed or there isn't already a boulder and it's in-bounds)
;; 2. see if there's a cycle
;; 3. collect the boulder locations for which there is a cycle

;; aka filter candidate boulders for which there is a cycle
;; candidate boulders are in front of the guard every step of the guard's path
;; if we're scared of infinite loops, max guard states is < 4x number of locations

(defn guard-states [{:keys [in-lab?
                            obstruction-locs
                            guard]
                     :as lab}]
  (let [move-guard (guard-mover obstruction-locs)]
    (sequence
     (take-while (comp in-lab? :position))
     (iterate move-guard guard))))

(defn candidate-obstructions [{:keys [in-lab?
                                      obstruction-locs
                                      guard]
                               :as lab}]
  (sets/difference
   (into #{}
         (comp
          (map next-guard-loc)
          (filter in-lab?))
         (guard-states lab))
   obstruction-locs
   #{(:position guard)}))

(defn seq-repeats? [s]
  (loop [acc #{}
         [head & tail] s]
    (when head
      (or
       (acc head)
       (recur (conj acc head) tail)))))

(defn lab-loops? [lab]
  (seq-repeats? (guard-states lab)))

(defn obstruct-lab [lab obstruction-loc]
  (update lab :obstruction-locs conj obstruction-loc))

(defn loops-guard? [lab obstruction]
  (-> lab
      (obstruct-lab obstruction)
      lab-loops?))

(defn solve-day-6-part-2 [lines]
  (let [lab (lab-from-lines lines)
        candidates (candidate-obstructions lab)
        loopers (filter #(loops-guard? lab %) candidates)]
    (count loopers)))


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
