(ns advent.y2019-robot-test
  (:require
   [advent.y2019 :refer :all]
   [advent.y2019-robot :refer :all]
   [clojure.test :refer :all]))


(comment
  (paint {:panels {}, :position [0 0], :brain {:output [0]}})
  ;; => [:any {:panels {[0 0] 0}, :position [0 0], :brain {:output ()}}];
  ((comp paint second paint) {:panels {}, :position [0 0], :brain {:output [0 1]}})
  ;; => [:any {:panels {[0 0] 1}, :position [0 0], :brain {:output ()}}];
  )

(comment
  (move :up [0 0]) ;; => [0 1];
  (move :down [0 0]) ;; => [0 -1];
  (move :left [0 0]) ;; => [-1 0];
  (move :right [0 0]) ;; => [1 0];
  )

(comment
  (= think think) ;; => true;
  (= think (get-in state-transitions [process-output :output-empty])) ;; => true;
  )

(comment
  (get-in state-transitions [:think :need-input]) ;; => :process-output;
  )

(comment
  (hull-bounds {[0 -1] 1, [-1 -1] 1, [1 0] 1, [1 1] 1, [0 0] 0}) ;; => {:left -1, :right 1, :top 1, :bottom -1};
  )

(comment
  (test-outputs [1 0 0 0 1 0 1 0 0 1 1 0 1 0])
  ;; => {:brain {:output (), :halted :finished, :input [0]},
  ;;     :position [0 1],
  ;;     :direction :left,
  ;;     :panels {[0 0] 0, [-1 0] 0, [-1 -1] 1, [0 -1] 1, [1 0] 1, [1 1] 1},
  ;;     :last-action :moved-last}
  (= "..#\n..#\n##." (hull-string (:panels (test-outputs [1 0 0 0 1 0 1 0 0 1 1 0 1 0]))))
  (= 6 (count (:panels (test-outputs [1 0 0 0 1 0 1 0 0 1 1 0 1 0]))))
  )

(comment
  (draw-hull (:panels final-state))
  ;; => Doesn't quite look alphanumeric....
  (keys final-state)
  (dissoc final-state :brain :panels)
  ;; => {:position [9 46], :direction :left, :last-action :moved-last}
  (dissoc (:brain final-state) :mem)
  ;; => {:pc 337, :input [1], :output (), :halted :finished}
  )
