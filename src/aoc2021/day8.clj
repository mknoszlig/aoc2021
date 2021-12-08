(ns day8
  (:require [clojure.string :as s]
            [clojure.set :as cs]))

(defn strs->sets [digit-strs]
  (->> (s/split digit-strs #" ")
       (mapv (fn [s] (reduce #(conj %1 (keyword (str %2))) #{} s)))))

(defn str->input [[patterns display]]
  {:digits (strs->sets patterns)
   :display (strs->sets display)})

(def input (->> (s/split-lines (slurp "resources/day8.txt"))
                (mapv #(s/split % #" \| "))
                (mapv str->input)
                ))


(def digit->segments
  {0 #{:a :b :c :e :f :g}
   1 #{:c :f}
   2 #{:a :c :d :e :g}
   3 #{:a :c :d :f :g}
   4 #{:b :c :d :f}
   5 #{:a :b :d :f :g}
   6 #{:a :b :d :e :f :g}
   7 #{:a :c :f}
   8 #{:a :b :c :d :e :f :g}
   9 #{:a :b :c :d :f :g}})

(defn seg-count [n]
  (fn [digits] (filter #(= n (count %)) digits)))

(defn is-size [n other]
  (fn [digits] (filter #(= n (count (cs/intersection % other)))
                       digits)))

(defn but-not [other]
  (fn [digits] (filter #(not= % other) digits)))

(defn d1 [digits]
  (first (-> digits ((seg-count 2)))))

(defn d4 [digits]
  (first (-> digits ((seg-count 4)))))

(defn d7 [digits]
  (first (filter #(= 3 (count %)) digits)))

(defn d8 [digits]
  (first (filter #(= 7 (count %)) digits)))

(defn d0 [digits]
  (-> digits
      ((seg-count 6))
      ((is-size 2 (d1 digits)))
      ((is-size 3 (d4 digits)))
      first))

(defn d2 [digits]
  (-> digits
      ((seg-count 5))
      ((is-size 2 (d4 digits)))
      first))

(defn d3 [digits]
  (-> digits
      ((seg-count 5))
      ((is-size 4 (d2 digits)))
      first))

(defn d5 [digits]
  (-> digits
      ((seg-count 5))
      ((is-size 3 (d2 digits)))
      first))

(defn d9 [digits]
  (-> digits
      ((seg-count 6))
      ((is-size 4 (d4 digits)))
      first))

(defn d6 [digits]
  (-> digits
      ((seg-count 6))
      ((but-not (d9 digits)))
      ((but-not (d0 digits)))
      first))

(defn deduce-digits [digits]
  {(d0 digits) 0
   (d1 digits) 1
   (d2 digits) 2
   (d3 digits) 3
   (d4 digits) 4
   (d5 digits) 5
   (d6 digits) 6
   (d7 digits) 7
   (d8 digits) 8
   (d9 digits) 9})

(defn part-1 [input]
  (->> (mapcat (fn [{:keys [digits display]}]
                 (mapv (deduce-digits digits) display))
               input)
       (filter #{1 4 7 8})
       count
       ))

(defn part-2 [input]
  (->> (mapv (fn [{:keys [digits display]}]
               (->> (mapv (deduce-digits digits) display)
                    (apply str)
                    (#(Integer/parseInt %))))
             input)
       (apply +)))

(printf "part 1: %s\n" (time (part-1 input)))
(printf "part 2: %s\n" (time (part-2 input)))
