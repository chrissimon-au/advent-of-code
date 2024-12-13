(ns day12-test 
  (:require
   [clojure.string :as string]
   [day12]))

(require '[clojure.test :as t])

(defn s [& strings] (string/join "\n" strings))

(t/deftest test-assertion
  
  (t/testing  "Single square"
    (t/is (= 4 (day12/fencing-price "o"))))
  
  (t/testing "Simple row"
    (t/is (= 12 (day12/fencing-price "oo")))) 
  
  (t/testing "grid"
    (t/is (= 32 (day12/fencing-price (s
                                      "oo"
                                      "oo")))))
  (t/testing "asymmetric grid"
    (t/is (= 50 (day12/fencing-price (s
                                      "ooo"
                                      "oo")))))
  (t/testing "test with holes"
    (t/is (= 128 (day12/fencing-price (s
                                       "ooo"
                                       "o.o"
                                       "ooo")))))
  
  (t/testing "test with two regions"
    (t/is (= 132 (day12/fencing-price (s
                                       "ooo"
                                       "oAo"
                                       "ooo")))))
  )

(test-assertion)