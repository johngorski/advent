(ns advent.y2019-arcade
  (:require
   [advent.y2019 :as y2019]
   [advent.y2019-intcode :as computer]
   [clojure.string :as string]))

(def arcade-outputs (:output (computer/run-intcode (computer/load-program (y2019/puzzle-in 13)))))

(def drawing-instructions
  (map
   (fn [[col row id]]
     {[col row] id})
   (partition 3 arcade-outputs)))

(def final-pixels (apply merge drawing-instructions))

(count
 (filter
  (fn [[loc id]] (= id 2))
  (seq final-pixels)))

;; Come back to part 2 later.
;; Not sure if the surest path is interactive or having the joystick dumbly track the ball.
;; Best to get stars elsewhere first, most likely.

