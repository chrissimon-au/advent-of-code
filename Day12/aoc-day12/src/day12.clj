(ns day12
  (:require
   [clojure.walk :as w]
   [clojure.repl :refer :all]
   [clojure.string :as string]))
  

(defn parse [gridMap] (mapv (fn [x] (string/split x #"")) (string/split gridMap #"\n")))

;(defn get-pos [region col row] (get (get region row) col))
(defn get-pos [iGrid col row] (filter #(and (= row (% :row)) (= col (% :col))) iGrid))

(defn get-value [iGrid col row] (let [cell (first (get-pos iGrid col row))]
                                  (if cell (cell :value) ".")))

(defn indexedGrid [grid] (for [[i row] (map-indexed list grid)
                               [j cell] (map-indexed list row)
                               :when (not= "." cell)]
                           {:col i :row j :value cell}))

(defn area [iRegion] (count iRegion))

(defn nc [value neighbour] (if (not= value neighbour) 1 0))

(defn elem-circumference [iRegion elem] (let [col (elem :col)
                                              row (elem :row)
                                              value (elem :value)
                                              east (get-value iRegion (+ col 1) row)
                                              west (get-value iRegion (- col 1) row)
                                              north (get-value iRegion col (- row 1))
                                              south (get-value iRegion col (+ row 1))]
                                          (+ (nc value east) (nc value west) (nc value south) (nc value north))))

(defn circumference
  [iRegion]
  (reduce + (map (partial elem-circumference iRegion) iRegion)))
  
(defn fencing-price-of-grid [iGrid] (* (area iGrid) (circumference iGrid)))

(defn fencing-price [gridMap] (
                               let [iGrid (indexedGrid (parse gridMap))]
                               (fencing-price-of-grid iGrid)))


(def ex "ooo\no.o\nooo")
(def exGrid (parse ex))
(def iGrid (indexedGrid exGrid))
(print iGrid)
(def e (first iGrid))
(print e)
(if (= 0 (e :row)) 1 0)
(+ 1 1 1)

(get-pos exGrid 0 0)
(.indexOf (get exGrid 0) "o")
(get exGrid 0)
(count (get exGrid 0))

(area iGrid)
(circumference iGrid)

(fencing-price ex)
(count [ [1 2 3] [1 2]])