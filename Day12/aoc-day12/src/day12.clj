(ns day12
  (:require
   [clojure.repl :refer :all] 
   [clojure.string :as string]))
  

(defn parse [gridMap] (mapv (fn [x] (string/split x #"")) (string/split gridMap #"\n")))

(defn get-cell [iGrid col row] (first (filter #(and (= row (% :row)) (= col (% :col))) iGrid)))

(defn get-cell-value [iGrid col row] (let [cell (get-cell iGrid col row)]
                                  (if cell (cell :value) ".")))

(defn indexedGrid [grid] (for [[i row] (map-indexed list grid)
                               [j cell] (map-indexed list row)
                               :when (not= "." cell)]
                           {:col i :row j :value (first (char-array cell))}))

(defn area [iRegion] (count iRegion))

(defn nc [value neighbour] (if (not= value neighbour) 1 0))

(defn elem-circumference [iRegion cell] (let [col (cell :col)
                                              row (cell :row)
                                              value (cell :value)
                                              east (get-cell-value iRegion (+ col 1) row)
                                              west (get-cell-value iRegion (- col 1) row)
                                              north (get-cell-value iRegion col (- row 1))
                                              south (get-cell-value iRegion col (+ row 1))]
                                          (+ (nc value east) (nc value west) (nc value south) (nc value north))))

(defn circumference
  [iRegion]
  (reduce + (map (partial elem-circumference iRegion) iRegion)))

(defn num-sides
  [iRegion]
  4)
  
(defn region-price [iRegion] (* (area iRegion) (circumference iRegion)))

(defn region-price-using-sides [iRegion] (* (area iRegion) (num-sides iRegion)))

(defn get-cell-if-in-region [iGrid cell col row] (
                                                  let [
                                                       value (cell :value)
                                                       neighbour (get-cell iGrid col row)
                                                       neighbourValue (get-cell-value iGrid col row)]
                                                  (when (= value neighbourValue)
                                                    neighbour)
))


(defn region-from-cell [iGrid cell] (if (= cell nil)
                                      ['() iGrid]
                                      (let [col (cell :col)
                                            row (cell :row)
                                            newGrid (remove #{cell} iGrid)
                                            east (get-cell-if-in-region newGrid cell (+ col 1) row)
                                            [eastTree newGrid] (region-from-cell newGrid east)
                                            west (get-cell-if-in-region newGrid cell (- col 1) row)
                                            [westTree newGrid] (region-from-cell newGrid west)
                                            north (get-cell-if-in-region newGrid cell col (- row 1))
                                            [northTree newGrid] (region-from-cell newGrid north)
                                            south (get-cell-if-in-region newGrid cell col (+ row 1))
                                            [southTree newGrid] (region-from-cell newGrid south)
                                            ]
                                       [(cons cell (concat eastTree westTree northTree southTree))
                                        newGrid])))
                                      
                                      
                                      

(defn split-into-regions [iGrid] (if (empty? iGrid)
                                   '()
                                   (let
                                    [cell (first iGrid)
                                     [region remainingGrid] (region-from-cell iGrid cell)
                                     ]
                                     (conj
                                      (split-into-regions remainingGrid)
                                      region
                                     )))
  )

(defn fencing-price [gridMap] (let [iGrid (indexedGrid (parse gridMap))
                                    regions (split-into-regions iGrid)]
                                (reduce + (map region-price regions))))

(defn fencing-price-using-sides [gridMap] (let [iGrid (indexedGrid (parse gridMap))
                                    regions (split-into-regions iGrid)]
                                (reduce + (map region-price-using-sides regions))))