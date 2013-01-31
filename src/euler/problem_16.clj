(ns euler.problem-16)

(defn expt [base pow]
  (apply *' (take pow (repeat base))))

(defn char->int [ch]
  (Integer/parseInt (str ch)))

(defn sum [coll]
  (apply +' coll))

(->> (expt 2 1000)
  str
  (map char->int)
  sum)


