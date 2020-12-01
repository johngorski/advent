(ns advent.y2020
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn puzzle-in [day] (slurp (io/resource (str "2020/" day ".txt"))))

(comment
  (def input (map edn/read-string (string/split (puzzle-in 1) #"\n")))

  (for [x input
        y input
        :when (= 2020 (+ x y))]
    [x y])
  ;; => ([1224 796]
  ;; [796 1224]);; => (974304 974304)

  (apply + [1224 796])
  (apply * [1224 796])
  ;; => 974304


  (for [x input
        y input
        z input
        :when (= 2020 (+ x y z))]
    (* x y z));; => (236430480 236430480 236430480 236430480 236430480 236430480)
  )
