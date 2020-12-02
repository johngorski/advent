(ns advent.y2020
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn puzzle-in [day] (slurp (io/resource (str "2020/" day ".txt"))))

(comment
  (def input (map edn/read-string (string/split (puzzle-in 1) #"\n")))

  (first
   (for [x input
         y input
         :when (= 2020 (+ x y))]
     (* x y)))
  ;; => 974304

  (apply + [1224 796])
  (apply * [1224 796])
  ;; => 974304


  (first
   (for [x input
         y input
         z input
         :when (= 2020 (+ x y z))]
     (* x y z)))
  ;; => 236430480
  )

(comment
  (def day2 (string/split (puzzle-in 2) #"\n"))
  (def day2 (puzzle-in 2))


  day2

  (def pwd-re #"(\d+)-(\d+) (\w): (\w+)")

  (re-matches pwd-re (first day2))
  ;; => ["5-6 s: zssmssbsms" "5" "6" "s" "zssmssbsms"]

  (frequencies "banana")
  ;; => {\b 1, \a 3, \n 2}

  (first "banana")
  ;; => \b

  (defn parse [entry]
    (let [[_ lo hi char pwd] (re-matches pwd-re entry)
          low (edn/read-string lo)
          high (edn/read-string hi)
          valid? (<= low
                     (get (frequencies pwd) (first char) 0)
                     high)]
      {:valid? valid?
       :password pwd
       :low low
       :high high
       :char (first char)}))

  (parse (first day2))

  (parse (second day2))

  (take 20 (map (comp :valid? parse) day2))

  (count (filter (comp :valid? parse) day2))
  ;; => 638


  (get "banana" 3)
  ;; => \a

  (defn parse-2 [entry]
    (let [[_ lo hi ch pwd] (re-matches pwd-re entry)
          idx1 (get pwd (dec (edn/read-string lo)))
          idx2 (get pwd (dec (edn/read-string hi)))
          char (first ch)
          match1 (= char idx1)
          match2 (= char idx2)
          valid? (and
                  (or match1 match2)
                  (not (and match1 match2)))]
      {:valid? valid?
       :password pwd
       :char char}))

  (count (filter (comp :valid? parse-2) day2));; => 699
  )
