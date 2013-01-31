(ns euler.problem-42
  (:require [clojure.string :as s]))

(defn t [n]
  (/ (* n (inc n))
     2))

(def triangle-numbers (map t (iterate inc 1)))

(defn triangle-number? [n]
  (some (partial = n)
        (take-while (partial >= n) triangle-numbers)))

(defn letter-score [ch]
  (- (-> ch s/lower-case first int)
     (-> \a int dec)))

(defn word-score [word]
  (apply +
    (map letter-score word)))

(def triangle-word? (comp triangle-number? word-score))

(def words
  (-> (slurp "/Users/simon/Code/euler/src/resources/problem-42.txt")
    (s/split #",")
    ((partial map #(s/replace % #"\"" "")))))

(count (filter triangle-word? words))
