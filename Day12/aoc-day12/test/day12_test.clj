(ns day12-test 
  (:require
   [clojure.string :as string]
   [day12]))

(require '[clojure.test :as t])

(defn long-str [& strings] (string/join "\n" strings))

(def t1 (long-str "oo"
                  "oo"))

(t/deftest test-assertion
  
  (t/testing  "Single square"
    (t/is (= 4 (day12/fencing-price "o"))))
  (t/testing "Simple row"
    (t/is (= 12 (day12/fencing-price "oo")))) 
  (t/testing "grid"
    (t/is (= 32 (day12/fencing-price t1)))))

(test-assertion)