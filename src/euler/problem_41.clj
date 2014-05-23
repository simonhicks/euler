(ns euler.problem-41)

(defn permutations
  [values]
  (loop [acc (map vector values)]
    (if (<= (count values) (count (first acc)))
      acc
      (recur (for [p acc, v values
                   :when (not (some #{v} p))]
               (conj p v))))))

(defn coll->int
  [nums]
  (Integer/parseInt (apply str nums)))

(defn pandigitals-to-n [n]
  (map coll->int (permutations (reverse (rest (range (inc n)))))))

(def pandigitals
  (mapcat pandigitals-to-n (reverse (rest (range 10)))))

(defn prime?
  [n]
  (let [lim (int (Math/ceil (Math/sqrt n)))]
    (not (some #(zero? (rem n %)) (range 2 (inc lim))))))

(time (first (filter prime? pandigitals)))
