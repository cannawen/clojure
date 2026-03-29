(ns aoc.2025.9.main
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn rotate [arr] (conj (vec (rest arr)) (first arr)))

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
    (if (= x1 x2)
      (->> (range y1 (inc y2))
           (map (fn [y] [x1 y])))
      (->> (range x1 (inc x2))
           (map (fn [x] [x y1]))))))

(pointsInBetween [[11 1] [11 9]])

(let [points (->> "aoc/2025/9/input-mini.txt"
                  slurp
                  string/split-lines
                  (map (fn [s] (mapv parse-long (string/split s #",")))))
      outline (->> points
               (map vector (rotate points))
               (mapcat pointsInBetween)
               set)
      maxX (->> points
                (map first)
                (apply max))
      maxY (->> points
                (map second)
                (apply max))]
  outline)

(rotate [1 2 3])
