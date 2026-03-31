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

    (->> (for [x (range x1 (inc x2))]
           (for [y (range y1 (inc y2))]
             [x y]))
         (apply concat))))

(points-between [[2 3] [11 7]])

(defn up [[x y]] [x (dec y)])
(defn down [[x y]] [x (inc y)])
(defn left [[x y]] [(dec x) y])
(defn right [[x y]] [(inc x) y])

(defn go [unvisited bounds current]
  (if (unvisited current)
    ;; visiting this point; remove from unvisited points
    (let [unvisited (disj unvisited current)]
      (if (bounds current)
        ;; if we are on a boundary, return
        unvisited
        ;; otherwise visit all adjacent points
        (-> unvisited
            (go bounds (up current))
            (go bounds (down current))
            (go bounds (left current))
            (go bounds (right current)))))
    ;; already visited this space; return
    unvisited))

(let [points (->> "aoc/2025/9/input.txt"
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
                (+ 2))
      yMax (->> points
                (map second)
                (apply max)
                (+ 2))
      outer-bounds (->> [[[xMin yMin] [xMin yMax]]
                         [[xMin yMax] [xMax yMax]]
                         [[xMax yMax] [xMax yMin]]
                         [[xMax yMin] [xMin yMin]]]
                        (mapcat points-between)
                        set)
      all-points (->>
                  (for [x (range xMin xMax)]
                    (for [y (range yMin yMax)]
                      [x y]))
                  (apply concat)
                  set)
      unvisited-points (go all-points (set/union shape-outline outer-bounds) [0,0])
      shape-points (-> unvisited-points
                       (set/union shape-outline)
                       ;; not sure why we need this, but we are getting [-2,-2] otherwise
                       (set/difference outer-bounds))

      rectangles (->> (for [p1 points]
                        (for [p2 points]
                          (if (= (first p1) (first p2))
                            (if (< (second p1) (second p2))
                              [p1 p2]
                              nil)
                            (if (< (first p1) (first p2))
                              [p1 p2]
                              nil))))
                      (apply concat)
                      (filter some?)
                      (map points-between)
                      (sort-by count >))]
  (->> rectangles
       (filter (fn [rectangle] (set/subset? rectangle shape-points)))
       (map count)
       (apply max)))
