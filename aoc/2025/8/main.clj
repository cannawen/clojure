(ns aoc.2025.8.main
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn distance [[v1 v2]]
  (apply + (map (fn [a b] (* (- a b) (- a b))) v1 v2)))

(let [points (->> "aoc/2025/8/input-mini.txt"
                  slurp
                  string/split-lines
                  (map (fn [s] (mapv parse-long (string/split s #",")))))
      connections (->> (for [a points
                             b points
                             :when (< 0 (compare a b))]
                         [a b])
                       (sort-by distance)
                       (map set))
      initial-circuits (map (fn [p] #{p}) points)]
  (->> connections
       (reductions (fn [memo connection]
                     ;; find all circuits in memo that overlap with connection and remove them from memo
                     ;; combine connection with the removed circuits and add back to memo
                     (let [{others true
                            overlapping false}
                           (group-by (fn [circuit] (empty? (set/intersection circuit connection))) memo)]
                       (with-meta
                         (conj others (apply set/union overlapping))
                         {:connection connection}))) 
                   initial-circuits)
       (filter (fn [circuits] (= 1 (count circuits))))
       first
       meta
       :connection
       (map first)
       (apply *)))
