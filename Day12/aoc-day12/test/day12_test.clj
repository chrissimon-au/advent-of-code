(ns day12-test 
  (:require
   [day12]))

(require '[clojure.test :as t])

(t/deftest test-assertion
  
  (t/testing  "Single square"
    (t/is (= (day12/fencing-price "o") 4))))

(test-assertion)