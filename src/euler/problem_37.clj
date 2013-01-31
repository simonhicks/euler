(ns euler.problem-37)

(defn factors
  [n]
  (filter #(zero? (mod n %)) (range 2 (inc (int (Math/sqrt n))))))

(def prime?
  (memoize
    (fn [n]
      (cond
        (< n 1) false
        (= n 2) true
        :else (empty? (factors n))))))

;; ways to reduce the search space for truncatable primes

; if there is an even digit in the number (aside from when the first digit is 2), then one of the truncations will be even hence it's not truncatable prime.

(defn has-an-even-truncation?
  [n]
  (let [[fst & rst] (str n)]
    (or
      (#{4 6 8} (int fst))
      (some (comp even? int) rst))))

; if any digit other than the first or last is 5, then one of the truncations will end in 5 hence it's not truncatable prime.
; if the last digit is 5 it's not prime.

(defn has-a-factor-of-5-trunction?
  [n]
  (let [[_ & rst] (str n)]
    (some #{\5} rst)))

; if the first or last digit is 9, then one of the truncations will be 9, hence it's not truncatable prime.
; if the first or last digit is 1, then one of the truncations will be 1, hence it's not truncatable prime.

(defn begins-or-ends-with-9?
  [n]
  (or
    (= \9 (first (str n)))
    (= \9 (last (str n)))))

(defn begins-or-ends-with-1?
  [n]
  (or
    (= \1 (first (str n)))
    (= \1 (last (str n)))))

(defmacro nor
  [& exprs]
  `(not (or ~@exprs)))

(defn worth-considering?
  [n]
  (nor (has-an-even-truncation? n)
       (has-a-factor-of-5-trunction? n)
       (begins-or-ends-with-9? n)
       (begins-or-ends-with-1? n)))

(defn truncations*
  [drop-fn n]
  (->> n str
    (iterate (partial drop-fn 1))
    (take-while (comp not empty?))
    (map (partial apply str))
    (map #(Integer/parseInt %))))

(defn truncations
  [n]
  (let [left-truncations (truncations* drop n)
        right-truncations (truncations* drop-last n)]
    (sort (set (concat left-truncations right-truncations)))))

(defn truncatable-prime?
  [n]
  (and
    (worth-considering? n)
    (every? prime? (truncations n))))

(apply + (take 11 (filter truncatable-prime? (iterate inc 10))))
