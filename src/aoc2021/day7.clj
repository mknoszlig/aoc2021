(ns day7
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

(def input (edn/read-string (format "[%s]" (slurp "resources/day7.txt"))))

(defn abs [x]
  (if (pos? x) x (- x)))

(defn fuel-at [m cost-fn pos]
  (reduce (fn [acc [p subs]]
            (+ acc
               (cost-fn subs (abs (- pos p)))))
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
          (min (cost-at-pos pmax)
               (cost-at-pos (dec pmax)))
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
