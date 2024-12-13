(ns day12
  (:require
   [clojure.walk :as w]
   [clojure.repl :refer :all]
   [clojure.string :as string]))
  

(defn parse [gridMap] (mapv (fn [x] (string/split x #"")) (string/split gridMap #"\n")))

(defn get-pos [iGrid col row] (filter #(and (= row (% :row)) (= col (% :col))) iGrid))

(defn get-value [iGrid col row] (let [cell (first (get-pos iGrid col row))]
                                  (if cell (cell :value) ".")))

(defn indexedGrid [grid] (for [[i row] (map-indexed list grid)
                               [j cell] (map-indexed list row)
                               :when (not= "." cell)]
                           {:col i :row j :value (first (char-array cell))}))

(defn cells-in-region [iGrid regionId] (filter #(= (% :value) regionId) iGrid))

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
  
(defn fencing-price-of-grid [iRegion] (* (area iRegion) (circumference iRegion)))

(defn region-ids [gridMap] (filter #(not= % \newline) (distinct gridMap)))

(defn fencing-price [gridMap] (
                               let [
                                    iGrid (indexedGrid (parse gridMap))
                                    regionIds (region-ids gridMap)
                                    regions (map (partial cells-in-region iGrid) regionIds)
                                    ]
                               (reduce + (map fencing-price-of-grid regions))
                              ))

(def ex "o")
(def iGrid (indexedGrid (parse ex)))
(def regionIds (region-ids ex))
(def regions (map (partial cells-in-region iGrid) regionIds))
(print regions)