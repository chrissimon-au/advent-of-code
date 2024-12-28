(ns day12
  (:require
   [clojure.repl :refer :all]
   [clojure.set :refer [union]]
   [clojure.string :as string]))
  

(defn parse [gridMap] (mapv (fn [x] (string/split x #"")) (string/split gridMap #"\n")))

(defn get-cell [iGrid col row] (first (filter #(and (= row (% :row)) (= col (% :col))) iGrid)))

(defn get-cell-value [iGrid col row] (let [cell (get-cell iGrid col row)]
                                  (if cell (cell :value) ".")))

(defn indexedGrid [grid] (for [[i row] (map-indexed list grid)
                               [j cell] (map-indexed list row)
                               :when (not= "." cell)]
                           {:col j :row i :value (first (char-array cell))}))

(defn area [iRegion] (count iRegion))

(defn neighbour-in-region [value neighbour] (if (not= value neighbour) 1 0))

(defn elem-circumference [iRegion cell] (let [col (cell :col)
                                              row (cell :row)
                                              value (cell :value)
                                              east (get-cell-value iRegion (+ col 1) row)
                                              west (get-cell-value iRegion (- col 1) row)
                                              north (get-cell-value iRegion col (- row 1))
                                              south (get-cell-value iRegion col (+ row 1))]
                                          (+
                                           (neighbour-in-region value east)
                                           (neighbour-in-region value west)
                                           (neighbour-in-region value south)
                                           (neighbour-in-region value north)
                                           )
                                          ))

(defn circumference
  [iRegion]
  (reduce + (map (partial elem-circumference iRegion) iRegion)))

(defn get-sides
  [sides cell direction]
  (first (filter (fn [side] (let
                      [cells (side :cells)]
                      (and (= (side :direction) direction) (contains? cells cell))
                      )) sides)))

(defn merge-sides 
  [side1 side2]
  (if (= nil side2)
    side1
    (if (= nil side1)
      side2
      {:direction (side1 :direction) :cells (union (side1 :cells) (side2 :cells))}
      )
   ))

(defn add-cell-to-side
  [sides cell neighbour1 neighbour2 direction]
  (let
   [
    neighbour1Side (get-sides sides neighbour1 direction)
    neighbour2Side (get-sides sides neighbour2 direction)
    neighbourSide (merge-sides neighbour1Side neighbour2Side)
   ]
  (if (= nil neighbourSide)
    (conj sides {:direction direction :cells #{cell}})
    (conj 
     (remove #{neighbour2Side} (remove #{neighbour1Side} sides))
     (assoc neighbourSide :cells (conj (neighbourSide :cells) cell))))
  )
)

(defn add-cell-to-sides
  [iRegion sides cell]
  (let [col (cell :col)
        row (cell :row)
        east (get-cell iRegion (+ col 1) row)
        west (get-cell iRegion (- col 1) row)
        north (get-cell iRegion col (- row 1))
        south (get-cell iRegion col (+ row 1))
        newSides (if (= nil north) (add-cell-to-side sides cell east west :north) sides)
        newSides (if (= nil south) (add-cell-to-side newSides cell east west :south) newSides)
        newSides (if (= nil east) (add-cell-to-side newSides cell north south :east) newSides)
        newSides (if (= nil west) (add-cell-to-side newSides cell north south :west) newSides)
        ]
    newSides
))

(defn num-sides
  [iRegion]
  (let
   [
    sides (reduce (fn [sides cell] (add-cell-to-sides iRegion sides cell)) '() iRegion)
   ]
    (count sides)
  )
  )
  
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

(def s1 #{6})
(def s2 #{7})
s1
s2
(union s1 s2)