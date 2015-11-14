(ns cloj.eval-test
  (:require [clojure.test :refer :all]
            [cloj.eval :refer :all]))

(deftest testing-constants
  (testing "constants"
    (is (= (my-eval "123") '(123)))
    (is (= (my-eval "1 2 3") '(1 2 3)))
    (is (= (my-eval "\"abc\"") '("abc")))
    ))

(deftest testing-ifs
  (testing "if expressions"
    (is (= (my-eval "(if 1 2 3)") '(2)))
    ))
