(ns advent.notebook
  (:require
   [nextjournal.clerk :as clerk]))

(defn -main [& args]
  (clerk/serve!
   {:browse? true
    :watch-paths ["src" "notebooks"]}))
