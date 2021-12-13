(ns day13
  (:require [clojure.edn :as edn]
            [clojure.string :as s]))

(def input (let [[points _ folds](->> (slurp "resources/day13.txt")
                                      s/split-lines
                                      (partition-by empty?))]
             {:points (mapv (comp edn/read-string (partial format "[%s]")) points)
              :folds  (->> (mapv (comp last #(s/split % #" ")) folds)
                           (mapv (comp edn/read-string #(s/replace % #"=" " ") (partial format "[%s]"))))}))


(defn fold [points [by v]]
  (let [pos (case by
              x 0
              y 1)]
    (->> (mapv (fn [p]
                 (update p pos #(if (<= % v) % (- v (- % v)))))
               points)
         (into #{}))))

(printf "part 1: %s\n" (time (-> (fold (:points input) (first (:folds input)))
                                 count)))


(def post-fold (time (reduce (fn [points f] (fold points f)) (:points input) (:folds input))))
(println "part 2:\n")
(doseq [y (range (inc (apply max (map last post-fold))))]
  (->> (range (inc (apply max (map first post-fold))))
       (mapv #(if (post-fold [% y]) "*" "."))
       (println)))

;; > time bb src/aoc2021/day13.clj
;; "Elapsed time: 1.299839 msecs"
;; part 1: 610
;; "Elapsed time: 6.709343 msecs"
;; part 2:

;; [* * * . . * * * * . * * * * . . . * * . * . . * . * * * . . * * * * . * * * *]
;; [* . . * . . . . * . * . . . . . . . * . * . . * . * . . * . * . . . . . . . *]
;; [* . . * . . . * . . * * * . . . . . * . * * * * . * . . * . * * * . . . . * .]
;; [* * * . . . * . . . * . . . . . . . * . * . . * . * * * . . * . . . . . * . .]
;; [* . . . . * . . . . * . . . . * . . * . * . . * . * . * . . * . . . . * . . .]
;; [* . . . . * * * * . * . . . . . * * . . * . . * . * . . * . * . . . . * * * *]

;; real	0m0.042s
;; user	0m0.026s
;; sys	0m0.011s

;; > time clojure -M src/aoc2021/day13.clj
;; "Elapsed time: 2.794346 msecs"
;; part 1: 610
;; "Elapsed time: 6.946941 msecs"
;; part 2:

;; [* * * . . * * * * . * * * * . . . * * . * . . * . * * * . . * * * * . * * * *]
;; [* . . * . . . . * . * . . . . . . . * . * . . * . * . . * . * . . . . . . . *]
;; [* . . * . . . * . . * * * . . . . . * . * * * * . * . . * . * * * . . . . * .]
;; [* * * . . . * . . . * . . . . . . . * . * . . * . * * * . . * . . . . . * . .]
;; [* . . . . * . . . . * . . . . * . . * . * . . * . * . * . . * . . . . * . . .]
;; [* . . . . * * * * . * . . . . . * * . . * . . * . * . . * . * . . . . * * * *]

;; real	0m1.020s
;; user	0m1.984s
;; sys	0m0.135s
