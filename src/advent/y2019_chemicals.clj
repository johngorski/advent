(ns advent.y2019-chemicals
  (:require
   [advent.y2019 :as y2019]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [instaparse.core :as instaparse]))

(def samples
  [[165
    "9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL"]
   [13312
    "157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"]
   [180697
    "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF"]
   [2210736
    "171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX"]])

(def formula-parser (instaparse/parser (slurp (io/resource "2019/reactions.ebnf"))))

(defn every-other [xs]
  (map first (partition-all 2 xs)))

(every-other (range 7)) ;; => (0 2 4 6);

(defn load-formulae [s]
  (instaparse/transform
   {:CHEMICAL keyword
    :QUANTITY edn/read-string
    :PRODUCT  (fn [quant _ chem] {chem quant})
    :REAGENTS (fn [& rs] (apply merge (every-other rs)))
    :FORMULA  (fn [reagents _ [[chem quant] & _]]
                {chem {:quantity quant
                       :reagents reagents}})
    :FORMULAE (fn [& fs] (apply merge (every-other fs)))
    }
   (formula-parser s)))

;; (load-formulae (y2019/puzzle-in 14))

;; Expect to cycle through with potentially some backtracking involved.
;; We likely end up with multiple possible paths with varying amounts of waste, though
;; we may not since each product has a unique formula. This isn't the sort of thing to
;; puzzle through at 1:30am, even if it is a weekend.
;; Keeping track of extras may be sufficient for part 1.

(defn batches-needed
  [quantity-needed batch-size]
  (+ (quot quantity-needed batch-size)
     (if (< 0 (rem quantity-needed batch-size)) 1 0)))

(comment
(batches-needed 1 1) ;; => 1;
(batches-needed 2 1) ;; => 2;
(batches-needed 3 2) ;; => 2;
(batches-needed 12 5) ;; => 3;
)

(defn reaction-step-from
  [quantity target formulae]
  (if (= :ORE target)
    quantity ;; 1 ORE = 1 ORE
    (let [formula    (formulae target)
          batch-size (:quantity formula)
          reagents   (:reagents formula)
          batches    (batches-needed quantity batch-size)]
      (->> (seq reagents)
           (map (fn [[ingredient needed]]
                  [(* batches needed) ingredient]))
           (into {})))))

(reaction-step-from 1 :ORE (load-formulae (second (samples 0)))) ;; => 1;

(let [fs (load-formulae (second (samples 0)))]
  (reaction-step-from 1 :FUEL fs)) ;; => ([2 :AB] [3 :BC] [4 :CA]);

(reaction-step-from 2 :AB (load-formulae (second (samples 0)))) ;; => ([6 :A] [8 :B]);
(reaction-step-from 3 :BC (load-formulae (second (samples 0)))) ;; => ([15 :B] [21 :C]);
(reaction-step-from 4 :CA (load-formulae (second (samples 0)))) ;; => ([16 :C] [4 :A]);
;; This starts to show where we need to combine to simplify.
;; If we combine here, we have ([10 :A] [23 :B] [37 :C])
;; Because of batch sizes, there will be a surplus of {:A 0, :B 1, :C 3}
;; There's no savings for the batches producing :A, but producing :C is
;; cheaper as 8 batches of 5 to make 37 than 5 batches for 21 + 4 batches for 16.
;; 

(defn make
  "Raw requirements to make the desired quantity of the target chemical given available formulae. "
  [quantity target formulae]
  (if (= :ORE target)
    quantity ;; 1 ORE = 1 ORE
    (let [formula    (formulae target)
          batch-size (:quantity formula)
          reagents   (:reagents formula)
          batches    (batches-needed quantity batch-size)]
      (reduce + (->> (seq reagents)
                     (map (fn [[ingredient needed]]
                            (make (* batches needed) ingredient formulae))))))))

(seq {:a 1}) ;; => ([:a 1]);
(load-formulae (second (samples 0)))

(comment
  (make 1 :ORE (load-formulae (y2019/puzzle-in 14))) ;; => 1;
  (make 1 :FUEL (load-formulae (second (samples 0)))) ;; => 172;
;; (make 1 :FUEL (load-formulae (y2019/puzzle-in 14)))
)

