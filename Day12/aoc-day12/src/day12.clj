(ns day12
  (:require
   [clojure.repl :refer :all]
   [clojure.string :as string]
  ))

(defn parse [gridMap] (string/split gridMap #"\n"))

(defn area [region] (reduce + (map count region)))

(defn row-circumference [row] (+ 2 (* 2 (count row))))

(defn circumference [region] (- (reduce + (map row-circumference region)) (* 2 (- (count region) 1) (count (get region 0)))))

(defn fencing-price-of-grid [grid] (* (area grid) (circumference grid)))

(defn fencing-price [gridMap] (fencing-price-of-grid (parse gridMap)))

(reduce 
 + [6 5])
()
(circumference (parse "ooo\nooo"))
(area (parse "ooo\nooo"))
(fencing-price "o")