(defn triangle? [x y z]
  (let [[a b c] (sort [x y z])]
    (< c (+ a b))))

(triangle? 5 4 1)
