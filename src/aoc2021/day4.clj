(ns day4
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

(defn wrap-edn-vec [str]
  (edn/read-string (format "[%s]" str)))

(defn prepare-board [b]
  (->> (concat b (apply map vector b))
       (mapv set)))

(defn parse-input [[h _ & boards]]
  {:numbers (wrap-edn-vec h)
   :drawn   []
   :boards  (->> (filter (comp not empty?) boards)
                 (mapv wrap-edn-vec)
                 (partition 5)
                 (mapv prepare-board))})

(def input (-> (s/split-lines (slurp "resources/day4.txt"))
               parse-input))


(defn bingo? [rowcols]
  (some empty? rowcols))

(defn score [{:keys [boards drawn]}]
  (* (last drawn)
     (->> (filter bingo? boards)
          first
          (apply concat)
          (into #{})
          (apply +))))

(defn update-board [rowcols draw]
  (mapv #(disj % draw) rowcols))

(defn step [{:keys [numbers drawn boards] :as state}]
  (let [[draw & more] numbers]
    (if draw
      {:numbers more
       :drawn  (conj drawn draw)
       :boards (mapv #(update-board % draw) boards)}
      state)))

(defn remove-winners [state]
  (update state :boards #(remove bingo? %)))

(defn find-bingo [init-state]
  (->> (iterate step init-state)
       (drop-while #(not-any? bingo? (:boards %)))
       first))

(defn find-last-bingo [init-state]
  (->> (iterate (comp remove-winners step) input)
       (drop-while #(> (count (:boards %)) 1))
       first
       find-bingo))

(printf "part 1: %s\n" (score (find-bingo input)))

(printf "part 2: %s\n" (score (find-last-bingo input)))
