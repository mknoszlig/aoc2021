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
  (reduce (fn [acc d] (+ acc (* boats d))) 0 (range 1 (inc distance))))

(defn min-fuel [input cost-fn]
  (->> (map (partial fuel-at (frequencies input) cost-fn) (range (apply max input)))
       (partition 2 1)
       (drop-while (partial apply >))
       ffirst))

(printf "part 1: %s\n" (time (min-fuel input *)))

(printf "part 2: %s\n" (time (min-fuel input exorbitant-cost)))

