(ns aoc.2025.9.main
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn rotate [arr] (conj (vec (rest arr)) (first arr)))
(defn rotate' [arr] (flatten [(rest arr) (first arr)]))

(defn pointsInBetween [points]
  (let [x1 (->> points 
                (map first)
                (apply min))
        x2 (->> points
                (map first)
                (apply max))
        y1 (->> points
                (map second)
                (apply min))
        y2 (->> points
                (map second)
                (apply max))]
    
    ))

(let [points (->> "aoc/2025/9/input-mini.txt"
                  slurp
                  string/split-lines
                  (map (fn [s] (mapv parse-long (string/split s #",")))))
      pairs (map vector points (rotate points))
      maxX (->> points
                (map first)
                (apply max))
      maxY (->> points
                (map second)
                (apply max))]
  pairs)

(rotate [1 2 3])
(rotate' [1 2 3])
