(ns euler.problem-38)

; calculates the product of i and each of the integers (1, 2, ... n) where n > 1 and concatenates
(defn string-concatenation
  [i n]
  (let [numbers (range 1 (inc n))]
    (apply str (map (partial * i) numbers))))

; PROBLEM:
;
; find the largest 9-digit pandigital (ie. a number containing each of the digits 1..9) that can be formed using the above `string-concatenation` function

; Reduce the set of numbers to search

; i must start with 9, since the first digit of the string concatenation for this question is the same as the first digit of the number

(defn begins-with-9?
  [n]
  (-> n str first (= \9)))

; if the string concatenation of i * 1 and i * 2 is more than 9 digits long, then i is too big.
(defn too-big?
  [n]
  (let [n2 (* 2 n)
      n-s (str n)
      n2-s (str n2)]
    (> (count (str n-s n2-s)) 9)))

; so, i must be less than this:
(def max-i (some #(and (too-big? %) %) (range)))

; values for i worth considering must be pandigital
(defn no-repeated-digits?
  [n]
  (let [number-of-characters (count (str n))]
    (= number-of-characters
       (count (set (str n))))))

(defn no-zeros?
  [n]
  (not (some (partial = \0) (str n))))

(defn pandigital?
  [n]
  (and (no-repeated-digits? n) (no-zeros? n)))

; only numbers which return true for this function are worth considering...
(defn worth-considering?
  [n]
  (and
    (pandigital? n)
    (begins-with-9? n)
    (<= n max-i)))

; n must be less than 5, since for i=9 and n=6 (the smallest case worth considering for n > 5) the string-concatenation is too long
; so for each value of i, these are the possible string concatenations

(defn all-concatenations [i]
  (let [multipliers (range 2 6)]
    (map (partial string-concatenation i) multipliers)))


(defn nine-digit-pandigital?
  [n]
  (and
    (-> n str count (= 9))
    (pandigital? n)))


; return any nine-digit-pandigitals that can be produced using this value for i
(defn nine-digit-pandigital-string-concatenations-from-i
  [i]
  (filter nine-digit-pandigital? (all-concatenations i)))

; brute force search :)
(let [candidate-i (filter worth-considering? (range (inc max-i)))
      candidate-res (map #(Integer/parseInt %) (flatten (map nine-digit-pandigital-string-concatenations-from-i candidate-i)))]
  (apply max candidate-res))

