(ns advent.y2020
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.spec.alpha :as spec]
   [instaparse.core :as insta]))

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

(comment
  "day 3"

  (take 20 (iterate #(rem (+ 3 %) 20) 0))

  (def day3 (-> (puzzle-in 3) (string/split #"\n")))

  (count (first day3));; => 31

  (map + [1 2 3] [4 5 6 7]);; => (5 7 9)

  (def slope (iterate #(rem (+ 3 %) (count (first day3))) 0))

  (take 20 slope)
  ;; => (0 3 6 9 12 15 18 21 24 27 30 2 5 8 11 14 17 20 23 26)

  (vec [1 2])

  (map #(vec [%1 %2]) day3 slope)

  (count (filter #(= \# %) (map #(nth %1 %2) day3 slope)));; => 225

  (defn slp [run]
    (iterate #(rem (+ run %) (count (first day3))) 0))

  (take 20 (slp 5));; => (0 5 10 15 20 25 30 4 9 14 19 24 29 3 8 13 18 23 28 2)

  (defn trees [run]
    (count (filter #(= \# %) (map #(nth %1 %2) day3 (slp run)))))

  (trees 3);; => 225

  (map trees [1 3 5 7]);; => (60 225 57 58)

  (def even-trees
    (count (filter #(= \# %) (map #(nth %1 %2)
                                  (map first (partition 1 2 day3))
                                  (slp 1)))))

  even-trees
  ;; => 25

  (map first (partition 1 2 (range 10)))
  ;; => (0 2 4 6 8)

  (* 25 60 225 57 58)
  ;; => 1115775000
  )

(comment
  "day 4"

  (def as-and-bs
    (insta/parser
     "S = AB*
     AB = A B
     A = 'a'+
     B = 'b'+"))

  (as-and-bs "aaaaabbbaaaabb")
  ;; => [:S [:AB [:A "a" "a" "a" "a" "a"] [:B "b" "b" "b"]] [:AB [:A "a" "a" "a" "a"] [:B "b" "b"]]]

  (def day4 (-> (puzzle-in 4) (string/split #"\n\n")))
  (last (butlast day4))
  ;; => "ecl:lzr\nhgt:177in eyr:2037 pid:175cm\nbyr:2023 hcl:03b398 iyr:2026"

  (def passport
    (insta/parser
     "passport = (field space)* field space?
    space = ' ' | '\n'
    field = key ':' value
    key = 'byr' | 'iyr' | 'eyr' | 'hgt' | 'hcl' | 'ecl' | 'pid' | 'cid'
    value = #'[^\\s]+'
    "))

  (passport (last (butlast day4)))
  (comment
    [:passport
     [:field [:key "ecl"] ":" [:value "lzr"]]
     [:space "\n"]
     [:field [:key "hgt"] ":" [:value "177in"]]
     [:space " "]
     [:field [:key "eyr"] ":" [:value "2037"]]
     [:space " "]
     [:field [:key "pid"] ":" [:value "175cm"]]
     [:space "\n"]
     [:field [:key "byr"] ":" [:value "2023"]]
     [:space " "]
     [:field [:key "hcl"] ":" [:value "03b398"]]
     [:space " "]
     [:field [:key "iyr"] ":" [:value "2026"]]])

  (passport (first day4))
  (comment
    [:passport
     [:field [:key "byr"] ":" [:value "1971"]]
     [:space "\n"]
     [:field [:key "ecl"] ":" [:value "hzl"]]
     [:space " "]
     [:field [:key "pid"] ":" [:value "112040163"]]
     [:space "\n"]
     [:field [:key "eyr"] ":" [:value "2023"]]
     [:space " "]
     [:field [:key "iyr"] ":" [:value "2019"]]
     [:space "\n"]
     [:field [:key "hcl"] ":" [:value "#b6652a"]]
     [:space " "]
     [:field [:key "hgt"] ":" [:value "167cm"]]])

  (get :q 0)

  (defn ->map [pprt]
    (let [fields (map first (partition 1 2 (drop 1 pprt)))
          ]
      (into {} (map (fn [[_ [_ key] _ [_ value]]] [key value]) fields))
      ))

  (->map (passport (first day4)))
  (comment
    {"byr" "1971",
     "ecl" "hzl",
     "pid" "112040163",
     "eyr" "2023",
     "iyr" "2019",
     "hcl" "#b6652a",
     "hgt" "167cm"})

  (keys (->map (passport (first day4))))
  ;; => ("byr" "ecl" "pid" "eyr" "iyr" "hcl" "hgt")

  (keys {:a 1 :b 2})
  set/difference
  disj

  (disj #{:a :b :c :d :e} :a :c :e)
  ;; => #{:b :d}

  (defn valid-passport? [pprt-map]
    (= #{} (apply (partial disj #{"byr" "ecl" "pid" "eyr" "iyr" "hcl" "hgt"}) (keys pprt-map))))

  ;; (count (filter (comp valid-passport? ->map passport) day4))

  ((comp valid-passport? ->map passport) (first day4))
  ;; => true

  (->> day4

       (map (comp ->map passport))


       (take 256) ;; Why is it the 257th fails while the first 256 are fine?

       )

  (->> (get day4 256)
       passport
       ->map)
  ;; => {"hgt" "165in", "ecl" "#db642f", "iyr" "2014", "eyr" "2020", "byr" "1955", "hcl" "371f72", "pid" "756089060"}

  ;; => [:passport [:field [:key "hgt"] ":" [:value "165in"]] [:space " "] [:field [:key "ecl"] ":" [:value "#db642f"]] [:space " "] [:field [:key "iyr"] ":" [:value "2014"]] [:space "\n"] [:field [:key "eyr"] ":" [:value "2020"]] [:space "\n"] [:field [:key "byr"] ":" [:value "1955"]] [:space " "] [:field [:key "hcl"] ":" [:value "371f72"]] [:space " "] [:field [:key "pid"] ":" [:value "756089060"]]]

  (->> (get day4 258)
       passport
       ->map)

  (count day4)
  ;; => 259

  (last day4)
  ;; => "iyr:2017 ecl:blu byr:1942 hcl:#733820 eyr:2023 hgt:151cm pid:289923625\n"

  (last (butlast day4))
  ;; => "ecl:lzr\nhgt:177in eyr:2037 pid:175cm\nbyr:2023 hcl:03b398 iyr:2026"

  (->> day4
       (map (comp ->map passport))
       (filter valid-passport?)
       count)
  ;; => 192

  (defn year-range [low high]
    (spec/and
     #(re-matches #"\d\d\d\d" %)
     #(<= low (edn/read-string %) high)
     ))

  (spec/def ::byr (year-range 1920 2002))

  (spec/valid? ::byr "1984")
  ;; => true

  (spec/def ::iyr (year-range 2010 2020))

  (spec/def ::eyr (year-range 2020 2030))

  (re-matches #"(\d+)(cm|in)" "190cm")
  ;; => ["190cm" "190" "cm"]

  (spec/def ::hgt
    #(when-let [[_ amount unit] (re-matches #"(\d+)(cm|in)" %)]
       (if (= "cm" unit)
         (<= 150 (edn/read-string amount) 193)
         (<= 59 (edn/read-string amount) 76))))

  (spec/valid? ::hgt "190cm")
  (spec/valid? ::hgt "72in")

  (spec/def ::hcl #(re-matches #"#[0-9a-f]{6}" %))

  (spec/valid? ::hcl "#1a2b3c");; => true

  (spec/def ::ecl #(re-matches #"(amb|blu|brn|gry|grn|hzl|oth)" %))

  (spec/valid? ::ecl "oth");; => true

  (spec/def ::pid #(re-matches #"\d{9}" %))

  (spec/def ::passport
    (spec/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]))

  (spec/valid? ::byr "1984")
  (spec/valid? ::iyr "2015")
  (spec/valid? ::eyr "2025")
  (spec/valid? ::hgt "190cm")
  (spec/valid? ::hcl "#00ff00")
  (spec/valid? ::ecl "brn")
  (spec/valid? ::pid "012345678")

  (spec/valid?
   ::passport
   {::byr "1984"
    ::iyr "2015"
    ::eyr "2025"
    ::hgt "190cm"
    ::hcl "#00ff00"
    ::ecl "brn"
    ::pid "012345678"})

  (spec/valid?
   ::passport
   {:byr "1984"
    :iyr "2015"
    :eyr "2025"
    :hgt "190cm"
    :hcl "#00ff00"
    :ecl "brn"
    :pid "012345678"})
  ;; => true

  (spec/valid?
   ::passport
   {:byr "1984"
    :iyr "2015"
    :eyr "2025"
    :hgt "190cm"
    :hcl "#00ff00"
    :ecl "brn"
    :pid "012345678"})

  (keyword "abc")
  ;; => :abc

  (defn -->map [pprt]
    (let [fields (map first (partition 1 2 (drop 1 pprt)))
          ]
      (into {} (map (fn [[_ [_ key] _ [_ value]]] [(keyword key) value]) fields))
      ))

  (->> day4
       (map (comp -->map passport))
       (filter #(spec/valid? ::passport %))
       count)
  ;; => 101
  )

(comment
  "day 5"

  (re-matches #"(\w{7})(\w{3})" "BFFFBBFRRR")

  (Integer/parseInt "1234" 5)
  (Integer/parseInt "0110" 2)

  (map {\F 0 \B 1, \L 0, \R 1} (seq "BFFFBBFRRR"))

  (defn seat->number [seat]
    (let [binary (apply str (map {\F 0 \B 1, \L 0, \R 1} (seq seat)))]
      (Integer/parseInt binary 2)))

  (seat->number "BFFFBBF")
  ;; => 70
  (seat->number "RRR")
  ;; => 7

  (defn seat-id [pass]
    (let [[_ r c] (re-matches #"(\w{7})(\w{3})" pass)
          row (seat->number r)
          column (seat->number c)]
      (+ (* 8 row) column))
    )

  (seat-id "BFFFBBFRRR")
  ;; => 567
  ;; => [70 7]

  (seat->number "BFFFBBFRRR")

  (seat-id "FFFBBBFRRR")
  ;; => 119
  ;; => [14 7]

  (seat->number "FFFBBBFRRR")
  ;; => 119

  (seat-id "BBFFBBFRLL")
  ;; => 820
  ;; => [102 4]

  (seat->number "BBFFBBFRLL")
  ;; => 820

  (def day5 (string/split (puzzle-in 5) #"\n"))

  (reduce max (map seat-id day5))
  ;; => 874

  (reduce min (map seat->number day5));; => 48

  (take 10 (sort (map seat-id day5)))
  ;; => (48 49 50 51 52 53 54 55 56 57)

  vector

  (def seats (sort (map seat-id day5)))
  (take 10 seats)

  (first
   (filter
    (partial apply not=)
    (map vector
         seats
         (map #(+ 48 %) (range))
         )));; => [595 594]
  )

