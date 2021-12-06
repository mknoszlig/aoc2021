(ns day6
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

(def input (edn/read-string (format "[%s]" (slurp "resources/day6.txt"))))

(defn update-state [state]
  (let [spawns (get state 0 0)]
    (-> (reduce #(assoc %1 (dec %2) (get %1 %2 0)) state (range 1 9))
        (update 6 + spawns)
        (assoc 8 spawns))))

(defn fish-after-gens [n]
  (->> (iterate update-state (frequencies input))
       (#(nth % n))
       vals
       (reduce +)))


(printf "part 1: %s\n" (time (fish-after-gens 80)))

(printf "part 2: %s\n" (time (fish-after-gens 256)))
