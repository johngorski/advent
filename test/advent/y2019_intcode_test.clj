(ns advent.y2019-intcode-test
  (:require
   [advent.y2019-intcode :refer :all]
   [clojure.test :refer :all]))

(comment
  (:output (run-intcode {:pc 0, :input [5], :mem in-5}))
  ;; 14110739
  )
;; end day 5

(comment
  (run-intcode {:pc 0 :mem (vec in-2)})
  (run-intcode {:pc 0
                :mem (-> (vec in-2)
                         (assoc 1 31)
                         (assoc 2 46))})
  (step {:pc 0
         :mem (-> (vec in-2)
                  (assoc 1 31)
                  (assoc 2 46))})
  )

(comment
  (run-intcode {:pc 0 :mem sample-2})
  (run-intcode {:pc 0 :mem [1,0,0,0,99]})
  (run-intcode {:pc 0 :mem [2,3,0,3,99]})
  (run-intcode {:pc 0 :mem [2,4,4,5,99]})
  (run-intcode {:pc 0 :mem [1,1,1,4,99,5,6,0,99]})

  (run-intcode {:pc 0 :mem (-> (vec in-2)
                               (assoc 1 12)
                               (assoc 2 2)
                               )})

  (run-intcode {:pc 0 :mem (-> (vec in-2)
                               (assoc 1 31)
                               (assoc 2 46))})
  )
