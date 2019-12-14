(ns advent.y2019-robot
  (:require
   [advent.y2019 :as y2019]
   [advent.y2019-intcode :as computer]))

;; Robot starts running its computer

;; Robot has a brain, position, and direction.
;; In the world, there's a robot and a bunch of panels.

(def brain (-> (y2019/puzzle-in 11) computer/load-program))

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

(def starting-world
  {:robot-brain     brain
   :robot-position  [0 0]
   :robot-direction :up
   :panels          {}
   :action          :painting})

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

