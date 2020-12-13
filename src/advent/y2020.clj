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

(comment
  "day 6"

  (def day6 (-> (puzzle-in 6) (string/split #"\n\n")))
  (take 10 day6)

  (count (set (filter #(not= \newline %) (seq "a\nc"))))

  (defn questions-yes'd [group]
    (count (set (filter #(not= \newline %) (seq group)))))

  (questions-yes'd "a\nc")

  (reduce + (map questions-yes'd day6))
  ;; => 6625

  (set/intersection #{:a})

  (defn all-yes'd [group]
    (->> (string/split group #"\n")
         (map #(into #{} (seq %)))
         (apply set/intersection)))

  (all-yes'd "abc")
  ;; => #{\a \b \c}

  (all-yes'd "a
b
c")
  ;; => #{}

  (all-yes'd "ab
ac")
  ;; => #{\a}

  (reduce + (map (comp count all-yes'd) day6));; => 3360
  )

(comment
  "day7"

  (def bag-rules-ast
    (insta/parser
     "rules = rule*
    rule = outer ' contain ' inners '.\\n'
    outer = adjective (' ' adjective)* #' bags?'
    inners = (inner (', ' inner)*) | 'no other bags'
    inner = #'\\d+' ' ' outer
    adjective = #'\\w+'
    "))

  (bag-rules-ast
   "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
")

  (def sample-rules
    (bag-rules-ast
     "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
"))

  [{["light" "red"] {["bright" "white"] 1
                     ["muted" "yellow"] 2}}
   ,,,]

  (defn translate-bag-rule [_ [_ []]]
    )

  (defmulti translate first)

  vector?

  (defmethod translate :rules [[_ & rules]]
    (into {} (map translate (filter vector? rules))))

  (defmethod translate :rule [[_ & tokens]]
    (let [[outer _ inners] tokens]
      [(translate outer) (translate inners)]))

  (defmethod translate :outer [[_ & adjs]]
    (map second (filter vector? adjs)))

  (translate [:outer [:adjective "light"] " " [:adjective "red"]])

  (mapcat identity (partition 1 2 (range 10)))
  ;; => (0 2 4 6 8)

  (apply concat (partition 1 2 (range 10)))

  (defmethod translate :inners [[& inners]]
    (into {} (map translate (filter vector? inners))))

  (partition 4 (range 20))
  ;; => ((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15) (16 17 18 19))

  (defmethod translate :inner [[_ quantity-str _ outer]]
    [(translate outer) (edn/read-string quantity-str)])

  (def translated
    (translate
     [:rules
      [:rule
       [:outer
        [:adjective "light"]
        " "
        [:adjective "red"]
        " bags"]
       " contain "
       [:inners
        [:inner
         "1"
         " "
         [:outer [:adjective "bright"] " " [:adjective "white"] " bag"]]
        ", "
        [:inner
         "2"
         " "
         [:outer [:adjective "muted"] " " [:adjective "yellow"] " bags"]]]
       ".\n"]
      [:rule [:outer [:adjective "dark"] " " [:adjective "orange"] " bags"] " contain " [:inners [:inner "3" " " [:outer [:adjective "bright"] " " [:adjective "white"] " bags"]] ", " [:inner "4" " " [:outer [:adjective "muted"] " " [:adjective "yellow"] " bags"]]] ".\n"]
      [:rule [:outer [:adjective "bright"] " " [:adjective "white"] " bags"] " contain " [:inners [:inner "1" " " [:outer [:adjective "shiny"] " " [:adjective "gold"] " bag"]]] ".\n"] [:rule [:outer [:adjective "muted"] " " [:adjective "yellow"] " bags"] " contain " [:inners [:inner "2" " " [:outer [:adjective "shiny"] " " [:adjective "gold"] " bags"]] ", " [:inner "9" " " [:outer [:adjective "faded"] " " [:adjective "blue"] " bags"]]] ".\n"] [:rule [:outer [:adjective "shiny"] " " [:adjective "gold"] " bags"] " contain " [:inners [:inner "1" " " [:outer [:adjective "dark"] " " [:adjective "olive"] " bag"]] ", " [:inner "2" " " [:outer [:adjective "vibrant"] " " [:adjective "plum"] " bags"]]] ".\n"] [:rule [:outer [:adjective "dark"] " " [:adjective "olive"] " bags"] " contain " [:inners [:inner "3" " " [:outer [:adjective "faded"] " " [:adjective "blue"] " bags"]] ", " [:inner "4" " " [:outer [:adjective "dotted"] " " [:adjective "black"] " bags"]]] ".\n"] [:rule [:outer [:adjective "vibrant"] " " [:adjective "plum"] " bags"] " contain " [:inners [:inner "5" " " [:outer [:adjective "faded"] " " [:adjective "blue"] " bags"]] ", " [:inner "6" " " [:outer [:adjective "dotted"] " " [:adjective "black"] " bags"]]] ".\n"]
      [:rule
       [:outer [:adjective "faded"] " " [:adjective "blue"] " bags"]
       " contain "
       [:inners "no other bags"] ".\n"]
      [:rule
       [:outer [:adjective "dotted"] " " [:adjective "black"] " bags"]
       " contain "
       [:inners "no other bags"] ".\n"]]))

  (translated (seq ["faded" "blue"]))
  (keys translated)

  (comment
    {("light" "red")
     {("bright" "white") 1, ("muted" "yellow") 2},

     ("muted" "yellow")
     {("shiny" "gold") 2,
      ("faded" "blue") 9},

     ("dotted" "black") {},

     ("bright" "white")
     {("shiny" "gold") 1},

     ("shiny" "gold")
     {("dark" "olive") 1, ("vibrant" "plum") 2},

     ("vibrant" "plum")
     {("faded" "blue") 5, ("dotted" "black") 6},

     ("dark" "olive")
     {("faded" "blue") 3, ("dotted" "black") 4},

     ("dark" "orange")
     {("bright" "white") 3, ("muted" "yellow") 4},

     ("faded" "blue") {}})

  (translated ["light" "red"])

  (contains? [10 20 30] 2)

  (last (butlast (bag-rules-ast (puzzle-in 7))))

  (defmulti translate-outards first)

  (set/union #{1} #{3})

  merge-with
  apply

  (defmethod translate-outards :rules [[_ & rules]]
    ((partial merge-with set/union) (map translate-outards (filter vector? rules))))

  (defmethod translate-outards :rule [_ & tokens]
    (let [[outer _ inners] tokens]
      (into
       {}
       (map
        (fn [inner] [inner (translate outer)])
        (translate-outards inners)))))

  (defmethod translate-outards :inners [[_ & inners]]
    (map translate-outards (filter vector? inners)))

  (defmethod translate-outards :inner [[_ _ _ outer]]
    (when outer (translate outer)))

  (translate sample-rules)
  (comment
    {("light" "red")
     {("bright" "white") 1, ("muted" "yellow") 2},

     ("muted" "yellow")
     {("shiny" "gold") 2, ("faded" "blue") 9},

     ("dotted" "black") {},

     ("bright" "white")
     {("shiny" "gold") 1},

     ("shiny" "gold")
     {("dark" "olive") 1, ("vibrant" "plum") 2},

     ("vibrant" "plum")
     {("faded" "blue") 5, ("dotted" "black") 6},

     ("dark" "olive")
     {("faded" "blue") 3, ("dotted" "black") 4},

     ("dark" "orange")
     {("bright" "white") 3, ("muted" "yellow") 4},

     ("faded" "blue") {}})

  (translate-outards sample-rules)

  (defn invert [translated-rules]
    (->> (seq translated-rules)
         (mapcat (fn [[outer inners]]
                   (map (fn [inner] {inner #{outer}}) (keys inners))))
         (apply (partial merge-with set/union))))

  (invert (translate sample-rules))
  (comment
    {("bright" "white")
     #{("light" "red") ("dark" "orange")},

     ("muted" "yellow")
     #{("light" "red") ("dark" "orange")},

     ("shiny" "gold")
     #{("muted" "yellow") ("bright" "white")},

     ("faded" "blue")
     #{("muted" "yellow") ("vibrant" "plum") ("dark" "olive")},

     ("dark" "olive")
     #{("shiny" "gold")},

     ("vibrant" "plum")
     #{("shiny" "gold")},

     ("dotted" "black")
     #{("vibrant" "plum") ("dark" "olive")}})

  (def inverted (invert (translate sample-rules)))

  (inverted ["shiny" "gold"])
  ;; => #{("muted" "yellow") ("bright" "white")}

  (concat nil [1 2])
  ;; => (1 2)

  (defn eventual-containers [inverted-rules inner-color seen]
    (if (seen inner-color)
      []
      (lazy-seq
       (let [inner-colors (inverted-rules inner-color)]
         (concat
          inner-colors
          (mapcat #(eventual-containers inverted-rules % (conj seen inner-color)) inner-colors))))))

  (take 10 (eventual-containers inverted ["shiny" "gold"] #{}))
  ;; => (("muted" "yellow") ("bright" "white") ("light" "red") ("dark" "orange") ("light" "red") ("dark" "orange"))

  (into #{} (eventual-containers inverted ["shiny" "gold"] #{}))

  (count (into #{} (eventual-containers inverted ["shiny" "gold"] #{})))
  ;; => 4

  (concat (inverted ["shiny" "gold"]) [3 4])
  ;; => (("muted" "yellow") ("bright" "white") 3 4)

  (count (into #{} (eventual-containers (invert (translate (bag-rules-ast (puzzle-in 7)))) ["shiny" "gold"] #{})))
  ;; => 124
  ;; day 7 part 1 answer. nice.

  (def day7
    (translate (bag-rules-ast (puzzle-in 7))))

  day7

  ;; dynamic programming sounds like a good idea for day 7, part 2
  ;; ...or, how about substituting with an accumulator + recur?

  (merge-with + {[1 2] 3} {'(1 2) 4})

  (map second {1 2})

  (defn map-values [f m]
    (into {} (map (fn [[k v]] [k (f v)])) m))

  (map-values inc {:a 1, :b 2})
  ;; => {:a 2, :b 3}

  (take 4 day7)
  (comment
    ([("clear" "green") {("light" "plum") 3, ("wavy" "lavender") 1, ("shiny" "olive") 1}]
     [("light" "teal") {("striped" "tomato") 5, ("drab" "teal") 5, ("shiny" "lavender") 5}]
     [("dim" "blue") {("dull" "cyan") 2, ("dull" "purple") 2, ("dark" "indigo") 1}]
     [("shiny" "blue") {("posh" "maroon") 3}]))

  ;; color :: [string]
  ;; quantity :: integer
  ;; bags :: {color quantity} / [[color quantity]]
  ;; inner-bags :: [{color quantity}]

  (defn open-bags [bag-rules [color quantity]]
    (->> (bag-rules color) ;; {color quantity}
         (map-values #(* quantity %))
         ))

  (open-bags day7 [["clear" "green"] 3])
  ;; => {("light" "plum") 9, ("wavy" "lavender") 3, ("shiny" "olive") 3}

  (open-bags day7 [["gespacho" "surprise"] 2])
  ;; => {}

  (map (partial open-bags day7) {["shiny" "gold"] 1})
  (comment
    ({("pale" "maroon") 2, ("pale" "purple") 5, ("posh" "brown") 4, ("dotted" "turquoise") 1}))

  (map (partial open-bags day7) {'("pale" "maroon") 2,
                                 '("pale" "purple") 5,
                                 '("posh" "brown") 4,
                                 '("dotted" "turquoise") 1})

  (apply merge-with + (map (partial open-bags day7) {'("pale" "maroon") 2,
                                                     '("pale" "purple") 5,
                                                     '("posh" "brown") 4,
                                                     '("dotted" "turquoise") 1}))

  (defn bags-inside [bag-rules bags-so-far bags]
    (if (empty? bags)
      bags-so-far
      (let [these-bags (reduce + (vals bags))
            inner-bags (map (partial open-bags bag-rules) bags)
            bags' (apply merge-with + inner-bags)]
        (recur bag-rules (+ bags-so-far these-bags) bags'))
      ))

  (bags-inside day7 -1 {["shiny" "gold"] 1})
  ;; => 34862
  )

(comment
  "day 9"

  (def day9 (-> (puzzle-in 9) (string/split #"\n") (->> (map edn/read-string))))

  day9

  (defn xmas? [preamble z]
    (first
     (for [x preamble
           :when (contains? (disj preamble x) (- z x))]
       [x ((disj preamble x) (- z x))])))

  (xmas? (set (range 1 26)) 26)
  ;; => [7 19]
  (xmas? (set (range 1 26)) 49)
  ;; => [24 25]
  (xmas? (set (range 1 26)) 100)
  ;; => nil
  (xmas? (set (range 1 26)) 50)
  ;; => nil

  (partition 3 1 (range 9))
  ;; => ((0 1 2) (1 2 3) (2 3 4) (3 4 5) (4 5 6) (5 6 7) (6 7 8))

  (defn xmas-partition? [part]
    (let [preamble (set (butlast part))
          x (last part)]
      (xmas? preamble x)))

  day9

  (xmas-partition? (range 26))
  ;; => [7 18]
  (last (first (remove xmas-partition? (partition 26 1 day9))));; => 3199139634

  (count day9)

  (type day9)
  (vec day9)

  (defn contiguous-range [target acc first-idx last-idx list]
    (cond
      (= target acc)
      (let [rng (subvec list first-idx (inc last-idx))]
        (println :found)
        (apply + (map #(apply % rng) [min max])))

      (>= first-idx last-idx)
      (let [first-idx' last-idx
            last-idx' (inc first-idx')
            acc' (+ (list first-idx') (list last-idx'))]
        (println :index)
        (recur target acc' first-idx' last-idx' list))

      (< acc target)
      (let [last-idx' (inc last-idx)]
        (println :too-low)
        (recur target (+ acc (list last-idx')) first-idx last-idx' list))

      (< target acc)
      (let [first-idx' (inc first-idx)]
        (println :too-high)
        (recur target (- acc (list first-idx)) first-idx' last-idx list))

      ))

  (contiguous-range 3199139634 0 0 0 (vec day9))
  ;; => 438559930

  (contiguous-range 127 (+ 35 20) 0 1
                    [35
                     20
                     15
                     25
                     47
                     40
                     62
                     55
                     65
                     95
                     102
                     117
                     150
                     182
                     127
                     219
                     299
                     277
                     309
                     576])
  ;; => 62
  )

(comment
  "day 10"

  (def day10
    (->
     (puzzle-in 10)
     (string/split #"\n")
     (->> (map edn/read-string))))

  (frequencies (map (fn [[a b]] (- b a)) (partition 2 1 (sort day10))))
  ;; => {1 67, 3 31}

  (* 67 31)
  ;; => 2077
  ;; too low

  (* 67 32)
  ;; => 2144
  ;; still too low

  (partition 2 1 (range 10))

  (last day10);; => 40
  (first day10);; => 84

  (def sample10
    [28
     33
     18
     42
     31
     14
     46
     20
     48
     47
     24
     23
     49
     45
     19
     38
     39
     11
     1
     32
     25
     35
     8
     17
     7
     9
     4
     2
     34
     10
     3])

  (frequencies
   (map
    (fn [[a b]] (- b a))
    (partition 2 1 (sort (conj sample10 0 (+ 3 (apply max sample10)))))))
  ;; => {1 22, 3 10}

  (defn j-diffs [js]
    (frequencies
     (map
      (fn [[a b]] (- b a))
      (partition 2 1 (sort (conj js 0 (+ 3 (apply max js))))))))

  (j-diffs sample10)

  (defn max-joltage [js]
    (apply * (map (j-diffs js) [1 3])))

  (max-joltage sample10);; => 220
  (max-joltage day10);; => 2176 ;; there we go
  (range 1 4)
  ;; => (1 2 3)
  (comment
    (def paths-to
      (memoize
       (fn [joltages joltage]
         (if (= 0 joltage)
           1
           (reduce
            +
            (map
             (partial paths-to joltages)
             (filter
              joltages
              (map
               #(- joltage %)
               (range 1 4)))))))))
    )

  (def paths-to
    (memoize
     (fn [joltages joltage]
       (cond
         (= 0 joltage)
         1

         (not (contains? joltages joltage))
         0

         :else
         (+ (paths-to joltages (- joltage 1))
            (paths-to joltages (- joltage 2))
            (paths-to joltages (- joltage 3)))))))

  (defn day10-part2 [js]
    (let [device-joltage (+ 3 (apply max js))]
      (paths-to (conj (set js) 0 device-joltage) device-joltage)))

  sample10
  (day10-part2 [16
                10
                15
                5
                1
                11
                7
                19
                6
                12
                4])
  ;; => 8


  (day10-part2 sample10)
  ;; => 19208

  (def fib
    (memoize
     (fn [n]
       (if (<= n 1)
         1
         (+ (fib (- n 1)) (fib (- n 2)))))))

  (map fib (range 10))
  ;; => (1 1 2 3 5 8 13 21 34 55)

  (day10-part2 day10)
  ;; => 18512297918464

  )

(comment
  "day 11"

  {\. :floor
   \L :empty-seat
   \# :occupied-seat}

  (def day11 (-> (puzzle-in 11) (string/split #"\n") (->> (mapv (comp vec seq)))))

  (count day11);; => 98
  (count (first day11));; => 96

  (defn occupied-neighbors [grid row col]
    (let [rows (count grid)
          cols (count (first grid))]
      (for [neighbor-row (filter #(< -1 % rows) (map #(+ row %) [-1 0 1]))
            neighbor-col (filter #(< -1 % cols) (map #(+ col %) [-1 0 1]))
            :when (and (not (and (= row neighbor-row) (= col neighbor-col)))
                       (= \# (get-in grid [neighbor-row neighbor-col])))]
        [neighbor-row neighbor-col])))

  (occupied-neighbors day11 0 0)

  (defn seat-round [grid]
    (let [rows (count grid)
          cols (count (first grid))]
      (vec (map-indexed (fn [row-idx row]
                          (vec (map-indexed (fn [col-idx col]
                                              (cond
                                                (and (= \L col) (empty? (occupied-neighbors grid row-idx col-idx)))
                                                \#

                                                (and (= \# col) (<= 4 (count (occupied-neighbors grid row-idx col-idx))))
                                                \L

                                                :else
                                                col))
                                            row)))
                        grid))))

  (first (first (drop-while #(apply not= %) (partition 2 1 [1 2 3 4 5 5 6]))))

  (seat-round day11)

  (comment
    (first (first (drop-while #(apply not= %) (partition 2 1 (iterate seat-round day11)))))
    )

  (mapcat identity [[1 2 3] [4 5 6]])
  ;; => (1 2 3 4 5 6)

  (comment
    (get (frequencies (mapcat identity (first (first (drop-while #(apply not= %) (partition 2 1 (iterate seat-round day11))))))) \#);; => 2418
    )

  (defn line-of-sight-seats [grid row-idx col-idx]
    (let [rows (count grid)
          cols (count (first grid))
          in-bounds? (fn [[r c]]
                       (and
                        (< -1 r rows)
                        (< -1 c cols)))
          dirs (for [dr [-1 0 1]
                     dc [-1 0 1]
                     :when (not (and (= 0 dr) (= 0 dc)))]
                 (fn [[cr cc]] [(+ cr dr) (+ cc dc)]))
          rays (map
                (fn [dir]
                  (take-while
                   in-bounds?
                   (iterate dir (dir [row-idx col-idx]))))
                dirs)

          ;; then get the first non-empty seat in each ray

          ]
      (filter some? (map (fn [ray] (first (remove #(= (get-in grid %) \.) ray))) rays))
      ))

  (line-of-sight-seats (seats
                        ".......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#.....") 4 3)
  ;; => ([2 1] [1 3] [0 7] [4 2] [4 8] [7 0] [8 3] [5 4])


  (line-of-sight-seats day11 0 0)
  ;; => ([0 1] [1 0] [1 1])
  (line-of-sight-seats day11 1 7)
  ;; => ([0 6] [0 7] [1 6] [1 9] [2 6] [2 7] [3 9])

  (defn occupied-line-of-seats [grid row-idx col-idx]
    (filter #(= \# %) (map #(get-in grid %) (line-of-sight-seats grid row-idx col-idx))))

  (occupied-line-of-seats day11 1 7)
  ;; => ()

  (defn line-of-seat-round [grid]
    (let [rows (count grid)
          cols (count (first grid))]
      (vec (map-indexed (fn [row-idx row]
                          (vec (map-indexed (fn [col-idx col]
                                              (cond
                                                (and (= \L col) (empty? (occupied-line-of-seats grid row-idx col-idx)))
                                                \#

                                                (and (= \# col) (<= 5 (count (occupied-line-of-seats grid row-idx col-idx))))
                                                \L

                                                :else
                                                col))
                                            1              row)))
                        grid))))

  (-> day11 line-of-seat-round line-of-seat-round line-of-seat-round)


  (def sample11 (-> "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL" (string/split #"\n") (->> (mapv (comp vec seq)))))

  (defn seats [seats-str]
    (-> seats-str (string/split #"\n") (->> (mapv (comp vec seq)))))

  (count (occupied-line-of-seats (seats ".......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#.....") 4 3))
  ;; => 2

  (occupied-line-of-seats (seats ".......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#.....") 4 3)
  ;; => (\# \#)



  (get (frequencies (mapcat identity (first (first (drop-while #(apply not= %) (partition 2 1 (iterate line-of-seat-round sample11))))))) \#)
  ;; => 26

  (get (frequencies (mapcat identity (first (first (drop-while #(apply not= %) (partition 2 1 (iterate line-of-seat-round day11))))))) \#)
  ;; => 2144

  )

(comment
  "day 12"

  {:heading 90
   :position [0 0]}

  (defn nav-instruction [s]
    (let [[_ action value] (re-matches #"(\w)(\d+)" s)]
      [action (edn/read-string value)]))

  (nav-instruction "F10")
  ;; => ["F" 10]

  (defn nav-decode
    "action + arg -> effect on the state of the ship (ship-state -> ship-state)"
    [action arg {:keys [heading] :as ship}]
    (case action
      "N"
      {:position (fn [[x y]] [x (+ y arg)])}

      "S"
      {:position (fn [[x y]] [x (- y arg)])}

      "E"
      {:position (fn [[x y]] [(+ x arg) y])}

      "W"
      {:position (fn [[x y]] [(- x arg) y])}

      "L"
      {:heading #(mod (- % arg) 360)}

      "R"
      {:heading #(mod (+ % arg) 360)}

      "F"
      (recur ({0 "N", 90 "E", 180 "S", 270 "W"} heading) arg ship)
      ))


  (merge-with #(%1 %2) ((partial nav-decode (nav-instruction "F10")) {:heading 90, :position [0 0]}) {:heading 90, :position [0 0]})

  (let [ship {:heading 90, :position [0 0]}
        [action arg] (nav-instruction "F10")]
    (merge-with #(%1 %2) (nav-decode action arg ship) ship))
  ;; => {:position [10 0], :heading 90}

  (let [ship {:heading 90, :position [0 0]}
        [action arg] (nav-instruction "N3")]
    (merge-with #(%1 %2) (nav-decode action arg ship) ship))
  ;; => {:position [0 3], :heading 90}

  (defn nav-step [ship instruction]
    (let [[action arg] instruction]
      (merge-with #(%1 %2) (nav-decode action arg ship) ship)))

  (def day12 (-> (puzzle-in 12) (string/split #"\n") (->> (map nav-instruction))))

  (take 10 day12)
  ;; => (["S" 1] ["R" 270] ["S" 5] ["W" 2] ["F" 63] ["S" 3] ["L" 90] ["W" 4] ["F" 59] ["S" 1])

  (reduce nav-step {:heading 90, :position [0 0]} day12)
  ;; => {:position [-743 576], :heading 270}
  (+ 743 576);; => 1319


  ;; 90-degree rotations
  {[3 4] [4 -3]
   [4 -3] [-3 -4]
   [-3 -4] [-4 3]
   [-4 3] [3 4]}

  (defn right-90 [[x y]]
    [y (- x)])

  {:position [0 0]
   :waypoint [10 1]}

  (defn waypoint-decode [action arg {:keys [waypoint] :as ship}]
    (case action
      "N"
      {:waypoint (fn [[x y]] [x (+ y arg)])}

      "S"
      {:waypoint (fn [[x y]] [x (- y arg)])}

      "E"
      {:waypoint (fn [[x y]] [(+ x arg) y])}

      "W"
      {:waypoint (fn [[x y]] [(- x arg) y])}

      "L"
      (recur "R" ({0 0, 90 270, 180 180, 270 90} arg) ship)

      "R"
      {:waypoint ({0 identity,
                   90 right-90,
                   180 (comp right-90 right-90),
                   270 (comp right-90 right-90 right-90)}
                  arg)}

      "F"
      {:position (fn [[x y]]
                   (let [[wx wy] waypoint]
                     [(+ x (* wx arg))
                      (+ y (* wy arg))]))}
      ))

  (defn waypoint-step [ship instruction]
    (let [[action arg] instruction]
      (merge-with #(%1 %2) (waypoint-decode action arg ship) ship)))

  (reduce waypoint-step {:position [0 0], :waypoint [10 1]} day12)
  ;; => {:position [-39392 23042], :waypoint [-65 -26]}

  (+ 39392 23042)
  ;; => 62434
  )

