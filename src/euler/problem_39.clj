(ns euler.problem-39)

; If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.
;
; {20,48,52}, {24,45,51}, {30,40,50}
;
; For which value of p  1000, is the number of solutions maximised?


(defn is-integral?
  [n]
  (< (- n (int n))
     0.000000000000000001))

(defn get-hypotenuse
  [a b]
  (Math/sqrt (+ (* a a)
                (* b b))))

(defn get-integral-hypotenuse
  [a b]
  (let [hyp (get-hypotenuse a b)]
    (when (is-integral? hyp) (int hyp))))

(defn make-triangle
  ([a b] (make-triangle a b (get-hypotenuse a b)))
  ([a b c]
    {:a a :b b :c c :p (+ a b c)}))

(defn make-integral-triangle
  [a b]
  (when-let [c (get-integral-hypotenuse a b)]
    (make-triangle a b c)))

; if we find every right angle triangle with one (non hypotenuse) side a, which has p < 1000,
; the maximum value of a to consider (MAX_A) is the smallest value of a for which a == b and p >= 1000
;   if a is greater than this number then either:
;     - b < a, in which case it's equivalent to another triangle we've already checked
;     - b >= a, in which case p > 1000
(def MAX_A
  (some #(and (<= 1000 (:p %)) (:a %))
    (filter (comp not nil?)
      (for [a (iterate inc 1)]
        (make-triangle a a)))))

(defn too-big?
  [triangle]
  (> (:p triangle) 1000))

; if check every a from 1 - MAX_A, we only need to check for a >= b
; because triangles with a < b are equivalent to other triangles we've already searched
(defn integral-right-angle-triangles-with-a-side-of-length [a]
  (take-while (comp not too-big?)
    (filter (comp not nil?)
      (for [b (range a 1000)]
        (make-integral-triangle a b)))))

(sort-by #(count (nth % 1))
  (group-by :p
    (apply concat
      (for [a (range 1 (inc MAX_A))]
        (integral-right-angle-triangles-with-a-side-of-length a)))))

(->> (for [a (range 1 (inc MAX_A))]
       (integral-right-angle-triangles-with-a-side-of-length a))
  (apply concat)
  (group-by :p)
  (sort-by #(count (nth % 1)))
  reverse
  first
  first)
