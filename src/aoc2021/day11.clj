(ns day11
  (:require [clojure.string :as s]
            [clojure.set :as cs]))

(def cavern (->> (s/split-lines (slurp "resources/day11.txt"))
                 (mapv (partial mapv (comp #(Integer/parseInt %) str)))))

(def adjacent
  (for [x (range -1 2)
        y (range -1 2)
        :when (not= 0 x y)]
    [x y]))

(defn lookup [cavern [x y]]
  (get-in cavern [y x]))

(defn change [f cavern [x y]]
  (update-in cavern [y x] f))

(def flash-trigger 9)

(def dimensions
  [(count (first cavern))
   (count cavern)])

(def coords (for [x (range (first dimensions))
                  y (range (last dimensions))]
              [x y]))

(defn neighbors [[x y]]
  (let [[xmax ymax] dimensions]
    (->> (map (fn [[x' y']] [(+ x x') (+ y y')]) adjacent)
         (filter (partial not-any? neg?))
         (filter (fn [[x y]] (and (< x xmax) (< y ymax)))))))

(defn triggered [cavern]
  (->> (map (juxt identity (partial lookup cavern)) coords)
       (filter (comp (partial < flash-trigger) last))
       (map first)
       (into #{})))

(defn reset-triggered [cavern coords]
  (reduce #(change (constantly 0) %1 %2) cavern coords))

(defn transfer-energy [cavern coord-updates]
  (reduce (fn [acc [coord n]] (change (partial + n) acc coord)) cavern coord-updates))

(defn step [[cavern flashes i]]
  (loop [seen   #{}
         cavern (reduce #(change inc %1 %2) cavern coords)]
    (if-let [triggered (seq (cs/difference (triggered cavern) seen))]
      (recur (into seen triggered)
             (transfer-energy cavern (-> (mapcat neighbors triggered)
                                         frequencies)))
      [(reset-triggered cavern seen) (+ flashes (count seen)) (inc i)])))

(defn all-triggered? [cavern]
  (every? zero? (apply concat cavern)))

(let [[cavern' flashes i] (time (nth (iterate step [cavern 0 0]) 100))]
  (printf "part 1: %s\n" flashes)
  (printf "part 2: %s\n"  (time (->> (iterate step [cavern' flashes i])
                                     (drop-while (comp not all-triggered? first))
                                     first
                                     last))))


;; > time bb src/aoc2021/day11.clj
;; "Elapsed time: 257.75328 msecs"
;; part 1: 1647
;; "Elapsed time: 365.561138 msecs"
;; part 2: 348

;; real	0m0.657s
;; user	0m0.585s
;; sys	0m0.063s

;; > time clojure -M src/aoc2021/day11.clj
;; "Elapsed time: 115.112707 msecs"
;; part 1: 1647
;; "Elapsed time: 210.404222 msecs"
;; part 2: 348

;; real	0m1.323s
;; user	0m3.830s
;; sys	0m0.205s
