(ns mini.playground-test 
  (:require
   [mini.playground]))

(require '[clojure.test :as t])

(t/deftest test-assertion
  

  (t/testing  "square is square"
    (t/is (= (mini.playground/square 5) 25) "true equals false")))

(test-assertion)