(ns day12)

(require '[clojure.repl :refer :all])

(defn area [region] (count region))

(defn circumference [region] (+ 2 (* 2 (count region))))

(defn fencing-price [gridMap] (* (area gridMap) (circumference gridMap)))
