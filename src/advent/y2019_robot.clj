(ns advent.y2019-robot
  (:require
   [advent.y2019 :as y2019]
   [advent.y2019-intcode :as computer]))

(def brain (-> (y2019/puzzle-in 11) computer/load-program))

;; State node functions, returning their result condition and the updated world.
;; Result conditions should be a member of the "condition" column of the state
;; transition table.
(defn think [{:keys [brain] :as world}]
  (let [brain'    (computer/run-intcode brain)
        world'    (assoc world :brain brain')
        condition (case (:halted brain')
                    :finished       :finished
                    :awaiting-input :need-input)]
    [condition world']))

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
)

(defn paint [{:keys [brain panels position] :as world}]
  (let [color   (first (:output brain))
        brain'  (update brain :output rest)
        panels' (assoc panels position color)
        world'  (assoc world
                       :brain  brain'
                       :panels panels')]
    [:any world']))

(defn move
  [direction [x y]]
  (case direction
    :up    [x (inc y)]
    :right [(inc x) y]
    :down  [x (dec y)]
    :left  [(dec x) y]))

(move :up [0 0]) ;; => [0 1];

(defn turn
  [command direction]
  (case command
    0 (case direction
        :up    :left
        :right :up
        :down  :right
        :left  :up)
    
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
                      (update :input append (current-panel world))
                      (dissoc :halted))
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

(state-machine :think starting-world)

(comment ;; pre-paper try
;; Robot has a brain, position, and direction.
;; In the world, there's a robot and a bunch of panels.



(defn colors-of
  [panels]
  (fn [position]
    (get panels position 0)))

(defn painter
  [position output]
  (fn [panels]
    (assoc panels position output)))

(defn mover
  [direction]
  (fn [[x y]]
    (case direction
      :up    #(vec [x (inc y)])
      :right #(vec [(inc x) y])
      :down  #(vec [x (dec y)])
      :left  #(vec [(dec x) y]))))

(defn turner
  [command]
  (case command
    0 (fn [direction]
        (case direction
          :up    :left
          :right :up
          :down  :right
          :left  :up))
    1 (fn [direction]
        (case direction
          :up    :right
          :right :down
          :down  :left
          :left  :up))))

(defn next-action
  [action]
  (if (= :painting action)
    :moving
    :painting))


(defn act-on-world
  "function transforming the world based on the list of commands"
  [[output & outputs] {:keys [robot-position robot-direction] :as world}]
  (if output
    (try
      (case (:action world)
        :painting
        (act-on-world outputs
                      (update (update world :action next-action) :panels (painter robot-position output)))
        :moving
        (let [direction' ((turner output)     robot-direction)
              position'  ((mover  direction') robot-position)]
          (act-on-world outputs (assoc (update world :action next-action)
                                        :robot-direction direction'
                                        :robot-position  position'))
          )
        )
      ;; world)
      (catch Exception e
        (println e)))
    world))

(defn paint-step
  "from the given robot position/direction and ship panels, returns the transform from the current round of output from the robot. The robot must paint. The robot must move and turn."
  [{:keys [robot panels]} robot-output]
  )

(defn paint-hull [robot-brain world]
  (let [halted-robot-brain (computer/run-intcode robot-brain) ;; Halts awaiting input or finished...or faulted
        world' (act-on-world (:output halted-robot-brain) world)
        input [((colors-of (:panels world')) (:robot-position world'))]
        refreshed-robot-brain (assoc (dissoc halted-robot-brain :output :halted) :input input)]
    (if (= :awaiting-input (:halted halted-robot-brain))
      (recur refreshed-robot-brain world')
       world')))

;;(paint-hull starting-world)


;; Process output to figure out what's been painted and what the robot's input is


;; Resume robot with cleared output and new input


;; Halts as finished => we're done

)
