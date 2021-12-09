(ns day9
  (:require [clojure.string :as s]
            [clojure.set :as cs]))

(def board (->> (s/split-lines (slurp "resources/day9.txt"))
                (mapv (partial mapv (comp #(Integer/parseInt %) str)))))

(def adjacent
  [[1 0] [-1 0] [0 1] [0 -1]])

(defn lookup [[x y]]
  (get-in board [y x]))

(def dimensions
  [(count (first board))
   (count board)])

(defn neighbors [[x y]]
  (let [[xmax ymax] dimensions]
    (->> (map (fn [[x' y']] [(+ x x') (+ y y')]) adjacent)
         (filter (partial not-any? neg?))
         (filter (fn [[x y]] (and (< x xmax) (< y ymax)))))))

(defn minimum? [point & [ignore?]]
  (let [neighs (->> (filter (complement (or ignore? {}))
                             (neighbors point))
                    (mapv lookup))]
    (when (seq neighs)
      (< (lookup point)
         (apply min neighs)))))

(def coords
  (let [[xmax ymax] dimensions]
    (mapcat #(mapv (partial vector %) (range ymax))
            (range xmax))))

(def minima (filter minimum? coords))

(defn fixpoint [f init]
  (->> (iterate f init)
       (reduce #(if (= %1 %2) (reduced %2) %2) nil)))

(defn extend-basin [basin-coords]
  (let [neighs (cs/difference (set (mapcat neighbors basin-coords))
                              basin-coords)]
    (if (empty? neighs)
      basin-coords
      (->> neighs
           (filter #(not= 9 (lookup %)))
           (reduce conj basin-coords)))))

(defn basins []
  (->> minima
       (map (comp set vector))
       (mapv (partial fixpoint (partial extend-basin)))
       (into #{})))

(defn risk-level []
  (->> minima
       (map (comp inc lookup))
       (apply +)))

(defn basin-product []
  (->> (basins)
       (mapv count)
       sort
       (reverse)
       (take 3)
       (apply *)))

(printf "part 1: %s\n" (time (risk-level)))

(printf "part 2: %s\n" (time (basin-product)))

;; > time bb src/aoc2021/day9.clj
;; "Elapsed time: 270.245863 msecs"
;; part 1: 475
;; "Elapsed time: 499.247925 msecs"
;; part 2: 1092012

;; real	0m0.901s
;; user	0m0.816s
;; sys	0m0.068s

;; > time clojure -M src/aoc2021/day9.clj
;; "Elapsed time: 83.341763 msecs"
;; part 1: 475
;; "Elapsed time: 199.919307 msecs"
;; part 2: 1092012

;; real	0m1.268s
;; user	0m3.594s
;; sys	0m0.189s
