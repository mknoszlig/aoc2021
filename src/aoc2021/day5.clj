(ns day5
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

(defn wrap-edn-vec [str]
  (edn/read-string (format "[%s]" str)))

(defn parse-coords [line]
  (mapv wrap-edn-vec (s/split line #" -> ")))

(def input (->> (s/split-lines (slurp "resources/day5.txt"))
                (mapv parse-coords)))

(defn diagonal? [[[x y] [x' y']]]
  (= (Math/abs (- x x'))
     (Math/abs (- y y'))))

(defn inc-range [x x']
  (let [mod (if (< x' x) dec inc)]
    (range x (mod x') (mod 0))))

(defn tips->points [[[x y] [x' y'] :as segment]]
  (if (diagonal? segment)
    (mapv vector (inc-range x x') (inc-range y y'))
    (for [rx (inc-range x x')
          ry (inc-range y y')]
      [rx ry])))

(defn intersections [coord-tips]
  (->> (mapcat tips->points coord-tips)
       frequencies
       vals
       (filter (partial < 1))
       count))

(printf "part 1: %s\n" (time (->> (filter (complement diagonal?) input)
                                  intersections)))

(printf "part 2: %s\n" (time (intersections input)))
