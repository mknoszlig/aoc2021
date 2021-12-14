(ns day14
  (:require [clojure.string :as s]))

(def input (let [[init _ rules](->> (slurp "resources/day14.txt")
                                    s/split-lines
                                    (partition-by empty?))]
             {:init (first init)
              :rules (->> rules (mapv #(s/split % #" -> "))
                          (into {}))}))

(def rules (:rules input))

(def init (:init input))

(defn tuple-freqs [input]
  (->> (partition 2 1 input)
       (mapv #(apply str %))
       (reduce #(update %1 %2 (fnil inc 0)) {})))

(defn letter-freqs [tuple-freqs]
  (-> (reduce (fn [acc [k v]]
                (update acc (str (first k))
                        (fnil (partial + v) 0)))
              {}
              tuple-freqs)
      (update (str (last init)) inc)))

(defn poly-step [tuple-freqs]
  (reduce (fn [acc [[th tt :as t] c]]
            (let [middle (get rules t)]
              (-> (update acc (str th middle) (fnil (partial + c) 0))
                  (update (str middle tt) (fnil (partial + c) 0)))))
          {}
   tuple-freqs))

(defn vmax [f]
  (->> (sort-by last f) last last))

(defn vmin [f]
  (->> (sort-by last f) first last))

(defn score [iterations]
  (let [freqs (-> (iterate poly-step (tuple-freqs init))
                  (nth iterations)
                 (letter-freqs))]
    (- (vmax freqs) (vmin freqs))))

(printf "part 1: %s\n" (time (score 10)))

(printf "part 2: %s\n" (time (score 40)))

;; > time bb src/aoc2021/day14.clj
;; "Elapsed time: 4.281429 msecs"
;; part 1: 3906
;; "Elapsed time: 21.777789 msecs"
;; part 2: 4441317262452

;; real	0m0.052s
;; user	0m0.033s
;; sys	0m0.016s

;; > time clojure -M src/aoc2021/day14.clj
;; "Elapsed time: 7.293864 msecs"
;; part 1: 3906
;; "Elapsed time: 8.746599 msecs"
;; part 2: 4441317262452

;; real	0m0.997s
;; user	0m2.049s
;; sys	0m0.166s
