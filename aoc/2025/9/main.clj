(ns aoc.2025.9.main
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn rotate [arr] (conj (vec (rest arr)) (first arr)))

(defn points-between [points]
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

(points-between [[11 1] [11 9]])

(defn up [[x y]] [x (dec y)])
(defn down [[x y]] [x (inc y)])
(defn left [[x y]] [(dec x) y])
(defn right [[x y]] [(inc x) y])

(defn go [unvisited bounds current]
  (let [newlyUnvisited (disj unvisited current)]
    (if (bounds current)
      newlyUnvisited))
  )

(let [points (->> "aoc/2025/9/input-mini.txt"
                  slurp
                  string/split-lines
                  (map (fn [s] (mapv parse-long (string/split s #",")))))
      shape-outline (->> points
                         (map vector (rotate points))
                         (mapcat points-between)
                         set)
      xMin (- 2)
      yMin (- 2)
      xMax (->> points
                (map first)
                (apply max)
                inc
                inc)
      yMax (->> points
                (map second)
                (apply max)
                inc
                inc)
      outerBounds (->> [[[xMin yMin] [xMin yMax]]
                        [[xMin yMax] [xMax yMax]]
                        [[xMax yMax] [xMax yMin]]
                        [[xMax yMin] [xMin yMin]]]
                       (mapcat points-between))
      all-points (for [x (range xMin xMax)] (for [y (range yMin yMax)] [x y]))]

  (go (set all-points) (set (concat shape-outline outerBounds)) [0,0]))

