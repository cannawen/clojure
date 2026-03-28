(ns aoc.2025.8.main
  (:require [clojure.string :as string]))

(->> "aoc/2025/8/input-mini.txt"
     slurp
     string/split-lines
     (map (fn [s] (map parse-long (string/split s #"," )))))
