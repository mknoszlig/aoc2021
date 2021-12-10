(ns day10
  (:require [clojure.string :as s]))

(def chunks (s/split-lines (slurp "resources/day10.txt")))

(def syntax-scores
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(def complete-raw-scores
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn complete-score [completed]
  (reduce (fn [acc c]
            (+ (* 5 acc)
               (complete-raw-scores c)))
          0 completed))

(def closing
  {\( \)
   \[ \]
   \{ \}
   \< \>})

(defn parse [chunk]
  (reduce (fn [[h & t :as stack] c]
            (cond (get closing c)
                  (cons c stack)
                  (= c (closing h))
                  t
                  :else
                  (reduced (syntax-scores c))))
          '()
          chunk))

(defn odd-median [s]
  (assert (odd? (count s)))
  (->> (sort s)
       (drop (int (/ (count s) 2)))
       first))

(defn complete [stack]
  (reduce #(conj %1 (closing %2)) [] stack))


(printf "part 1: %s" (time (->> (map parse chunks)
                                (filter number?)
                                (apply +))))

(printf "part 2: %s" (time (->> (map parse chunks)
                                (filter seq?)
                                (map (comp complete-score complete))
                                odd-median)))

;; > time bb src/aoc2021/day10.clj
;; "Elapsed time: 21.641443 msecs"
;; part 1: 341823"Elapsed time: 23.06314 msecs"
;; part 2: 2801302861
;; real	0m0.070s
;; user	0m0.048s
;; sys	0m0.018s

;; > time clojure -M src/aoc2021/day10.clj
;; "Elapsed time: 4.864418 msecs"
;; part 1: 341823"Elapsed time: 4.454686 msecs"
;; part 2: 2801302861
;; real	0m0.972s
;; user	0m1.842s
;; sys	0m0.132s
