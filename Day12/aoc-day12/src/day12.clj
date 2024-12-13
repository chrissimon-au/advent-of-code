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
                           {:col i :row j :value (first (char-array cell))}))

(defn cells-in-region [iRegion regionId] (filter #(= (% :value) regionId) iRegion))

(defn area [iRegion regionId] (count (cells-in-region iRegion regionId)))

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
  [iRegion regionId]
  (reduce + (map (partial elem-circumference iRegion) (cells-in-region iRegion regionId))))
  
(defn fencing-price-of-grid [iGrid regionId] (* (area iGrid regionId) (circumference iGrid regionId)))

(defn region-ids [gridMap] (filter #(not= % \newline) (distinct gridMap)))

(defn fencing-price [gridMap] (
                               let [
                                    iGrid (indexedGrid (parse gridMap))
                                    regionIds (region-ids gridMap)
                                    ]
                               (reduce + (map (partial fencing-price-of-grid iGrid) regionIds))
                              ))


(def ex "ooo\noAo\nooo")

(def exGrid (parse ex))
(def iGrid (indexedGrid exGrid))


(region-ids ex)
(cells-in-region iGrid "o")
(area iGrid "A")
(circumference iGrid "A")
(= \a "a")

(first (char-array "a"))
(fencing-price-of-grid iGrid \o)
(fencing-price ex)
(count [ [1 2 3] [1 2]])