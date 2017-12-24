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

(testing "boolean functions"
    (is (= (my-eval "(not #t)") '(false)))
    (is (= (my-eval "(not #f)") '(true)))
    (is (= (my-eval "(boolean? #t)") '(true)))
    )

  (testing "map function"
    (is (= (my-eval "(map zero? (list))") '(())))
    (is (= (my-eval "(map zero? (list 0))") '((true))))
    (is (= (my-eval "(map zero? (list 0 1))") '((true false))))
    (is (= (my-eval "(map (lambda (x) (+ 1 x)) (list 0 1))") '((1 2)))))

  (testing "foldl function"
    (is (= (my-eval "(foldl + 0 (list 1 2 3))") '(6))))

  (testing "foldr function"
    (is (= (my-eval "(foldr + 0 (list 1 2 3))") '(6)))))

(run-tests)