(ns day1
  (:require [clojure.string :as s]))

(def ms
  (->> (slurp "resources/day1.txt")
       s/split-lines
       (mapv #(Integer/parseInt %))))

(defn count-increases [ms window-size]
  (->> ms
       (partition window-size 1)
       (map (partial apply +))
       (partition 2 1)
       (filter (partial apply <))
       count))

(println (count-increases ms 1))
(println (count-increases ms 3))
