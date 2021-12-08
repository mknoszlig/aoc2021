(ns day8
  (:require [clojure.string :as s]
            [clojure.set :as cs]))

(defn strs->sets [digit-strs]
  (->> (s/split digit-strs #" ")
       (mapv (fn [s] (reduce #(conj %1 (keyword (str %2))) #{} s)))))

(defn str->input [[patterns display]]
  {:digits  (strs->sets patterns)
   :display (strs->sets display)})

(def input (->> (s/split-lines (slurp "resources/day8.txt"))
                (mapv #(s/split % #" \| "))
                (mapv str->input)))

(def segments->digit
  {#{:a :b :c :e :f :g}    0
   #{:c :f}                1
   #{:a :c :d :e :g}       2
   #{:a :c :d :f :g}       3
   #{:b :c :d :f}          4
   #{:a :b :d :f :g}       5
   #{:a :b :d :e :f :g}    6
   #{:a :c :f}             7
   #{:a :b :c :d :e :f :g} 8
   #{:a :b :c :d :f :g}    9})

(defn is-matches? [mapping p p']
  (->> mapping
       (drop-while (fn [[s s']] (= (count (cs/intersection p s))
                                   (count (cs/intersection p' s')))))
       (empty?)))

(defn join-sorted-by-size [d d']
  (let [groupd  (group-by count d)
        groupd' (group-by count d')
        max     (apply max (keys groupd))]
    (->> (map (juxt groupd groupd') (range (inc max)))
         (filter (partial not= [nil nil]))
         (take max))))

(defn map-intersections [seed ds ds']
  (reduce (fn [acc [d d']]
            (if (is-matches? acc d d')
              (assoc acc d d')
              acc))
          seed
          (mapcat #(mapv (partial vector %) ds') ds)))

(defn map-sub-groups [seed [d d']]
  (cond
    (= 1 (count d) (count d'))
    (assoc seed (first d) (first d'))
    :else
    (map-intersections seed d d')))

(defn map-to-origin [m]
  (reduce (fn [acc [d d']]
                   (assoc acc d' (segments->digit d)))
          {}
          m))

(defn deduce-digits [digits]
  (->> (reduce map-sub-groups {} (join-sorted-by-size (keys segments->digit) digits))
       (map-to-origin)))

(defn part-1 [input]
  (->> (mapcat (fn [{:keys [digits display]}]
                 (mapv (deduce-digits digits) display))
               input)
       (filter #{1 4 7 8})
       count))

(defn part-2 [input]
  (->> (mapv (fn [{:keys [digits display]}]
               (->> (mapv (deduce-digits digits) display)
                    (apply str)
                    (#(Integer/parseInt %))))
             input)
       (apply +)))

(printf "part 1: %s\n" (time (part-1 input)))
(printf "part 2: %s\n" (time (part-2 input)))

;; > time clojure -M src/aoc2021/day8.clj
;; "Elapsed time: 55.161213 msecs"
;; part 1: 261
;; "Elapsed time: 52.043766 msecs"
;; part 2: 987553

;; real	0m1.128s
;; user	0m2.525s
;; sys	0m0.182s

;; > time bb src/aoc2021/day8.clj
;; "Elapsed time: 63.358904 msecs"
;; part 1: 261
;; "Elapsed time: 72.496108 msecs"
;; part 2: 987553

;; real	0m0.179s
;; user	0m0.135s
;; sys	0m0.039s
