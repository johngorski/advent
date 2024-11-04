(ns advent.puzzle
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as sets]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [instaparse.core :as insta]))

(defn in-lines
  ([year day] (in-lines identity year day))
  ([xd year day]
   (with-open [reader (io/reader (io/resource (format "%d/%d.txt" year day)))]
     (into [] xd (line-seq reader)))))

(comment
  (in-lines (take 2) 2023 1))

