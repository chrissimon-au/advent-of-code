(ns mini.playground-test)

(require '[clojure.test :as t])

(t/deftest test-assertion
  (t/testing "true is true"
    (t/is (true? true) "true is true"))

  (t/testing  "true equals false"
    (t/is (= true true) "true equals false")))

(test-assertion)