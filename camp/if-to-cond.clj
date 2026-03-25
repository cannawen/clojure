(defn process-value
  [value]
  (cond
    (string? value) :a-string
    (not (number? value)) :something-else
    (> value 10) :pretty-big
    (< value 0) :negative
    (zero? value) :zero
    :else :small-number))

(process-value "1")
