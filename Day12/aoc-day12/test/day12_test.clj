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
  
  (t/testing "test with multiple separate regions with same ID"
    (t/is (= 772 (day12/fencing-price (s
                                       "OOOOO"
                                       "OXOXO"
                                       "OOOOO"
                                       "OXOXO"
                                       "OOOOO"))))) 
  
  (t/testing "AoC Part 1 Sample"
    (t/is (= (Integer/parseInt(slurp "sampledata.answer.txt"))
             (day12/fencing-price (slurp "sampledata.txt"))))) 

  ;; (t/testing "AoC Part 1 Test"
  ;;   (t/is (= (Integer/parseInt (slurp "testdata.answer.txt"))
  ;;            (day12/fencing-price (slurp "testdata.txt")))))

  (t/testing "simple with sides"
    (t/is (= 8
             (day12/fencing-price-using-sides "oo"))))
  
    (t/testing "more complex num of sides"
    (t/is (= (+ (* 5 8) (* 1 4))
             (day12/fencing-price-using-sides (s
                                               "oo"
                                               "oA"
                                               "oo")))))
  
  (t/testing "more complex num of sides"
    (t/is (= 368
             (day12/fencing-price-using-sides (s
                                               "AAAAAA"
                                               "AAABBA"
                                               "AAABBA"
                                               "ABBAAA"
                                               "ABBAAA"
                                               "AAAAAA")))))
  
  (t/testing "AoC Part 2 Sample"
    (t/is (= (Integer/parseInt (slurp "sampledata.answer2.txt"))
             (day12/fencing-price-using-sides (slurp "sampledata.txt"))))) 
    
  ;; (t/testing "AoC Part 2 Test"
  ;;   (t/is (= (Integer/parseInt (slurp "testdata.answer2.txt"))
  ;;            (day12/fencing-price-using-sides (slurp "testdata.txt")))))
  )

(test-assertion)