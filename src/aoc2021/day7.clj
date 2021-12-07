(ns day7
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

(def input (edn/read-string (format "[%s]" (slurp "resources/day7.txt"))))

(defn fuel-at [m cost-fn pos]
  (reduce (fn [acc [p subs]]
            (+ acc
               (cost-fn subs (Math/abs (- pos p)))))
          0
          m))

(defn exorbitant-cost [boats distance]
  (* boats
     (/ (* distance (inc distance))
        2)))

(defn bin-search [cost-at-pos pmin pmax]
  (let [middle (int (/ (+ pmax pmin) 2))]
    (cond (= pmin middle pmax)
          (cost-at-pos pmin)
          (= 1 (- pmax pmin))
          (cost-at-pos (inc pmin))
          (< (cost-at-pos pmin)
             (cost-at-pos pmax))
          (recur cost-at-pos pmin middle)
          :else
          (recur cost-at-pos middle pmax))))

(defn min-fuel [input cost-fn]
  (bin-search (partial fuel-at (frequencies input) cost-fn)
              0
              (apply max input)))

(printf "part 1: %s\n" (time (min-fuel input *)))

(printf "part 2: %s\n" (time (min-fuel input exorbitant-cost)))
