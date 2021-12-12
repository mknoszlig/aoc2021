(ns day12
  (:require [clojure.string :as s]
            [clojure.set :as cs]))

(def adjacency (->> (slurp "resources/day12.txt")
                    (s/split-lines)
                    (mapv #(s/split % #"-"))
                    (reduce (fn [adj [k v]]
                              (-> adj
                                  (update k (fnil  conj #{}) v)
                                  (update v (fnil  conj #{}) k)))
                            {})))

(defn small? [node]
  (>= 123 (int (first node)) 97))

(defn small-visit-once [options _ small-seen?]
  (cs/difference options small-seen?))

(defn one-double-visit [options path small-seen?]
  (let [dupe (-> (reduce (fn [acc v]
                          (if (acc v)
                            (reduced {:found v})
                            (conj acc v)))
                        #{}
                        path)
                 :found)]
    (if dupe
      (cs/difference options (conj small-seen? dupe))
      (disj options "start"))))

(defn paths [adjacency from to found path small-seen? options-fn]
  (let [options (options-fn (get adjacency from) path small-seen?)]
    (cond (= from to)
          (conj found path)
          (empty? options)
          nil
          :else
          (reduce (fn [found opt]
                    (->> (paths adjacency
                                opt
                                to
                                #{}
                                (conj path opt)
                                (if (small? opt)
                                  (conj small-seen? opt)
                                  small-seen?)
                                options-fn)
                         (into found)))
                  found
                  options))))

(def run-search (partial paths adjacency "start" "end" #{} ["start"] #{"start"}))

(printf "part 1: %s\n" (time (-> (run-search small-visit-once )
                                 count)))

(printf "part 2: %s\n" (time (-> (run-search one-double-visit)
                                 count)))

;; > time clojure -M src/aoc2021/day12.clj
;; "Elapsed time: 85.243511 msecs"
;; part 1: 4573
;; "Elapsed time: 850.618809 msecs"
;; part 2: 42153

;; real	0m1.995s
;; user	0m7.294s
;; sys	0m0.439s

;; > time bb src/aoc2021/day12.clj
;; "Elapsed time: 122.877459 msecs"
;; part 1: 4573
;; "Elapsed time: 1276.133082 msecs"
;; part 2: 42153

;; real	0m1.433s
;; user	0m1.338s
;; sys	0m0.082s
