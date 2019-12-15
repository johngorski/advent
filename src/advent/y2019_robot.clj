(ns advent.y2019-robot
  (:require
   [advent.y2019 :as y2019]
   [advent.y2019-intcode :as computer]
   [clojure.string :as string]))

(def brain (-> (y2019/puzzle-in 11) computer/load-program))

;; State node functions, returning their result condition and the updated world.
;; Result conditions should be a member of the "condition" column of the state
;; transition table.
(defn think [{:keys [brain] :as world}]
  (if (#{:finished :crashed} (:halted brain))
    [:finished world]
    (let [brain'    (computer/run-intcode (dissoc brain :halted))
          world'    (assoc world :brain brain')
          condition (case (:halted brain')
                      :finished       :need-input ;; This is now poorly-named, since it's now a signal to process output.
                      :awaiting-input :need-input)]
      [condition world'])))

(defn append
  "A conj which treats nil as an empty vector"
  [coll x]
  (if (empty? coll)
    [x]
    (vec (conj coll x))))

(comment
(rest []) ;; => ();
(update (update {} :input conj 0) :input conj 1) ;; => {:input (1 0)};
(update {} :input append 0) ;; => {:input [0]};
(update (update {} :input append 0) :input append 1) ;; => {:input [0 1]};
)

(defn current-panel [{:keys [position panels] :as world}]
  (get panels position 0))

(comment
(current-panel {:panels {} :position [0 0]}) ;; => 0;
(current-panel {:panels {[0 0] 1} :position [0 0]}) ;; => 1;
(current-panel {:panels {[0 0] 0} :position [0 0]}) ;; => 0;
)

(defn paint [{:keys [brain panels position] :as world}]
  (let [color   (first (:output brain))
        brain'  (update brain :output rest)
        panels' (assoc panels position color)
        world'  (assoc world
                       :brain  brain'
                       :panels panels')]
    [:any world']))

(paint {:panels {}, :position [0 0], :brain {:output [0]}}) ;; => [:any {:panels {[0 0] 0}, :position [0 0], :brain {:output ()}}];
((comp paint second paint) {:panels {}, :position [0 0], :brain {:output [0 1]}}) ;; => [:any {:panels {[0 0] 1}, :position [0 0], :brain {:output ()}}];

(defn move
  [direction [x y]]
  (case direction
    :up    [x (inc y)]
    :right [(inc x) y]
    :down  [x (dec y)]
    :left  [(dec x) y]))

(move :up [0 0]) ;; => [0 1];
(move :down [0 0]) ;; => [0 -1];
(move :left [0 0]) ;; => [-1 0];
(move :right [0 0]) ;; => [1 0];

(defn turn
  [command direction]
  (case command
    0 (case direction
        :up    :left
        :left  :down
        :down  :right
        :right :up)
    
    1 (case direction
        :up    :right
        :right :down
        :down  :left
        :left  :up)))

(defn turn-move [{:keys [brain position direction] :as world}]
  (let [command    (first (:output brain))
        brain'     (update brain :output rest)
        direction' (turn command direction)
        position'  (move direction' position)
        world'     (assoc world
                          :brain         brain'
                          :direction direction'
                          :position   position')]
    [:any world']))

(defn process-output [{:keys [brain last-action] :as world}]
  (if (empty? (:output brain))
    (let [brain'  (-> brain
                      (update :input append (current-panel world)))
          world'  (assoc world :brain brain')
          condition :output-empty]
      [condition world'])
    (let [last-action' (case last-action
                         :painted-last :moved-last
                         :moved-last   :painted-last)]
      [last-action (assoc world :last-action last-action')])))

(def state-transitions
  ;; start -> condition -> next
  {:think          {:need-input   :process-output
                   :finished      :done}
   :process-output {:output-empty :think
                   :moved-last    :paint
                   :painted-last  :turn-move}
   :paint          {:any          :process-output}
   :turn-move      {:any          :process-output}})

(def effect
  {:think          think
   :process-output process-output
   :paint          paint
   :turn-move      turn-move})

(comment
(= think think) ;; => true;
(= think (get-in state-transitions [process-output :output-empty])) ;; => true;
)

(defn state-machine
  "Transitions from the current state to the next state until it's done, outputting the final state of the world."
  [state world]
  (if (= :done state)
    world
    (let [[condition world'] ((effect state) world)
          next               (get-in state-transitions [state condition]
                                     (fn [_]
                                       (throw (ex-info "State transition missing" {:state state :condition condition }))))]
      (recur next world'))))

(get-in state-transitions [:think :need-input]) ;; => :process-output;

(def starting-world
  {:brain       brain
   :position    [0 0]
   :direction   :up
   :panels      {}
   :last-action :moved-last})

;; (state-machine :think starting-world)
(def final-state (state-machine :think starting-world))

(defn clip-state [world]
  (-> world
    (dissoc :panels)
    (update :brain #(dissoc % :mem))))

(defn hull-bounds [panels]
  (let [painted (keys panels)]
    {:left   (apply min (map first painted))
     :right  (apply max (map first painted))
     :top    (apply max (map second painted))
     :bottom (apply min (map second painted))}))

(hull-bounds {[0 -1] 1, [-1 -1] 1, [1 0] 1, [1 1] 1, [0 0] 0}) ;; => {:left -1, :right 1, :top 1, :bottom -1};

(defn draw-hull [panels]
  (let [{:keys [left right top bottom]} (hull-bounds panels)]
    (for [x (range left (inc right))
          y (range top (dec bottom) -1)]
      (do
        (when (= 0 x)
          (print "\n"))
        (print (if (= 1 (get panels [x y]))
                 "#"
                 "."))))))

(defn hull-string
  "Strings representing rows in the hull joined with newlines."
  [panels]
  (let [{:keys [left right top bottom]} (hull-bounds panels)]
    (->>
     (for [y (range top (dec bottom) -1)]
       (->> (range left (inc right))
            (map (fn [x] (if (= 1 (get panels [x y])) "#" ".")))
            (string/join "")))
     (string/join "\n"))))

(defn test-outputs
  [outputs]
  (state-machine :process-output (assoc starting-world :brain {:output outputs :halted :finished} :last-action :moved-last)))

(test-outputs [1 0 0 0 1 0 1 0 0 1 1 0 1 0])
;; => {:brain {:output (), :halted :finished, :input [0]},
;;     :position [0 1],
;;     :direction :left,
;;     :panels {[0 0] 0, [-1 0] 0, [-1 -1] 1, [0 -1] 1, [1 0] 1, [1 1] 1},
;;     :last-action :moved-last}

(defn draw-hull [panels]
  (println (hull-string panels)))

(draw-hull (:panels final-state)) ;; => Doesn't quite look alphanumeric....

;; Come back to day 11 another time, especially if I find Intcode computer problems....
