(ns day12
  (:require
   [clojure.repl :refer :all]
   [clojure.string :as string]))
  

(defn parse [gridMap] (string/split gridMap #"\n"))

(defn area [region] (reduce + (map count region)))

(defn circumference
  [region]
  (+ (* 2 (count region))
     (* 2 (count (get region 0)))
  )
)
(defn fencing-price-of-grid [grid] (* (area grid) (circumference grid)))

(defn fencing-price [gridMap] (fencing-price-of-grid (parse gridMap)))


(circumference (parse "o"))
(area (parse "ooo\noo"))
(fencing-price "o")
(fencing-price "oo")
(fencing-price "oo\noo")
(fencing-price "ooo\noo")