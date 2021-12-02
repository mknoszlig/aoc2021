(ns day2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse-cmd [l]
  (let [[cmd amt] (s/split l #"\s")]
    [(keyword cmd) (Integer/parseInt amt)]))

(def commands
  (->> (io/resource "day2.txt")
       io/reader
       (line-seq)
       (mapv parse-cmd)))

(defn command-multiple [cmds reduce-fn init]
  (->> (reduce reduce-fn init cmds)
       (take 2)
       (apply *)))


;; part 1

(def moves
  {:forward [1 0]
   :up      [0 -1]
   :down    [0 1]})

(defn move [[x y] [direction amount]]
  {:post [(not-any? neg? %)]}
  (let [[mx my] (get moves direction)]
    [(+ x (* mx amount))
     (+ y (* my amount))]))


;; part 2

(def aim-moves
  {:forward [1 1 0]
   :up      [0 0 -1]
   :down    [0 0 1]})

(defn aim-and-move [[x y a] [direction amount]]
  {:post [(not-any? neg? %)]}
  (let [[mx my ma] (get aim-moves direction)]
    [(+ x (* mx amount))
     (+ y (* my a amount))
     (+ a (* ma amount))]))


(printf "part 1: %s\n"
        (command-multiple commands move [0 0]))

(printf "part 2: %s\n"
        (command-multiple commands aim-and-move [0 0 0]))
