(defn classify-triangle [a b c]
  (cond
    (and a == b a == c) :equilateral
    :else :invalid))

(classify-triangle 1 1 1)
:=
:equilateral
(classify-triangle 0 0 0)
:=
:invalid
(classify-triangle 1 1 3)
:=
:invalid
(classify-triangle 4 4 5)
:=
:isosceles
(classify-triangle 3 4 5)
:=
:scalene
