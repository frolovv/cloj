(ns cloj.std-test
  (:require [clojure.test :refer :all]
            [cloj.eval :refer :all]))

(deftest testing-standard-library
  (testing "numeric functions"
    (is (= (my-eval "(zero? 0)") '(true)))
    (is (= (my-eval "(zero? 1)") '(false)))
    (is (= (my-eval "(positive? 1)") '(true)))
    (is (= (my-eval "(negative? 1)") '(false)))
    (is (= (my-eval "(even? 0)") '(true)))
    (is (= (my-eval "(even? 2)") '(true)))
    (is (= (my-eval "(odd? 0)") '(false)))
    (is (= (my-eval "(odd? 1)") '(true)))
    (is (= (my-eval "(null? (list))") '(true)))
    (is (= (my-eval "(null? 123)") '(false))))
    
  (testing "map function"
    (is (= (my-eval "(map (list) zero?)") '(())))
    (is (= (my-eval "(map (list 0) zero?)") '((true))))
    (is (= (my-eval "(map (list 0 1) zero?)") '((true false))))
    (is (= (my-eval "(map (list 0 1) (lambda (x) (+ 1 x)))") '((1 2))))
    ))