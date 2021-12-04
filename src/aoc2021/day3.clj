(ns day3
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "resources/day3.txt")))


(defn bit-freqs [bin-strs]
  (->> (apply map vector bin-strs)
       (map frequencies)))

(defn base2->10 [bits]
  (Integer/parseInt (apply str bits) 2))

(defn vmin [freqs]
  (if (apply = (vals freqs))
    \0
    (-> (sort-by last freqs)
        ffirst)))

(defn vmax [freqs]
  (case (vmin freqs)
    \0 \1
    \1 \0))

;; part 1

(defn gamma-rate [bit-freqs]
  (-> (map vmax bit-freqs)
      base2->10))

(defn epsilon-rate [bit-freqs]
  (-> (map vmin bit-freqs)
      base2->10))

(defn power-consumption [report-data]
  (let [freqs (bit-freqs report-data)]
    (* (epsilon-rate freqs)
       (gamma-rate freqs))))


;; part 2

(defn bit-criterion [crit-fn bit-freqs n]
  (crit-fn (nth bit-freqs n)))

(defn select-with-bit [report-data pos expected]
  (filter #(= expected (nth % pos))
          report-data))

(defn apply-criterion [crit-fn report-data pos]
  (select-with-bit report-data
                   pos
                   (bit-criterion crit-fn (bit-freqs report-data) pos)))

(defn exhaust-criterion [crit-fn data]
  (->> (iterate (fn [[d i]]
                  [(apply-criterion crit-fn d i) (inc i)])
                [data 0])
       (drop-while #(> (count (first %)) 1))
       ffirst
       first))

(defn oxygen-generator-rating [data]
  (-> (exhaust-criterion vmax data)
      base2->10))

(defn co2-scrubber-rating [data]
  (-> (exhaust-criterion vmin data)
      base2->10))

(defn life-support-rating [data]
  (* (oxygen-generator-rating data)
     (co2-scrubber-rating data)))

(printf "power consumption: %s\n" (power-consumption input))
(printf "life support rating: %s\n" (life-support-rating input))
