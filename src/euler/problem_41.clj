(ns euler.problem-41)

(def divisible-by?
  (memoize
    (fn [x n] (zero? (rem x n)))))

(defn prime?
  [x]
  (let [limit (int (Math/sqrt x))]
    (not
      (some (partial divisible-by? x)
          (range 2 (inc limit))))))

(defmacro pandigital-numbers [number-of-digits]
  (let [d-sym (memoize (fn [n] (gensym (symbol (str "d" n "_")))))
        digits-sym (gensym "digits")]
    (letfn [(conditions-for-digit [n]
              (cons `and
                (for [d (range 1 n)]
                  `(not= ~(d-sym d)
                         ~(d-sym n)))))

            (bindings-for-digit [n]
              (list (d-sym n) digits-sym
                    :when (conditions-for-digit n)))

            (bindings-for-all-digits [n]
              (->> (for [x (range 1 (inc n))]
                    (bindings-for-digit x))
                (apply concat)
                vec))

            (code [n] `(Integer/parseInt (str ~@(map d-sym (range 1 (inc n))))))]
     `(let [~digits-sym (range 1 (inc ~number-of-digits))]
        (for ~(bindings-for-all-digits number-of-digits) ~(code number-of-digits))))))

(defmacro largest-pandigital-prime-with-n-digits [n]
  `(first (filter prime? (reverse (pandigital-numbers ~n)))))

(time
  (or
    (largest-pandigital-prime-with-n-digits 9)
    (largest-pandigital-prime-with-n-digits 8)
    (largest-pandigital-prime-with-n-digits 7)
    (largest-pandigital-prime-with-n-digits 6)
    (largest-pandigital-prime-with-n-digits 5)
    (largest-pandigital-prime-with-n-digits 4)
    (largest-pandigital-prime-with-n-digits 3)
    (largest-pandigital-prime-with-n-digits 2)))
