(ns day1
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def ms
  (->> (io/resource "day1.txt")
       io/reader
       (line-seq)
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
