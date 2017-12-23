(ns cloj.expand-test
  (:require [clojure.test :refer :all]
            [cloj.ast :refer :all]
            [cloj.expand :refer :all]))

(deftest test-non-expandable
  (testing "forms that should not be expanded"
    (is (= (ast "123") (expand (ast "123"))))
    (is (= (ast "123 456") (expand (ast "123 456"))))

    (is (= (ast "abc") (expand (ast "abc"))))
    (is (= (ast "(if 1 2 3)") (expand (ast "(if 1 2 3)"))))
    (is (= (ast "(+ 1 2 3)") (expand (ast "(+ 1 2 3)"))))
    (is (= (ast "(lambda (x) x)") (expand (ast "(lambda (x) x)"))))

    (is (= (ast "(and 1 2 3)") (expand (ast "(and 1 2 3)"))))
    (is (= (ast "(or 1 2 3)") (expand (ast "(or 1 2 3)"))))
    (is (= (ast "(define x 10)") (expand (ast "(define x 10)"))))))

(deftest test-expandable
  (testing "let expressions"
    (is (= (expand (ast "(let ((x 1)) x)")) (ast "((lambda (x) x) 1)"))))
  (testing "let* expressions"
    (is (= (expand (ast "(let* ((x 1)) x)")) (ast "((lambda (x) x) 1)")))
    (is (= (expand (ast "(let* ((x 1) (y x)) y)")) (ast "((lambda (x) ((lambda (y) y) x)) 1)"))))
  (testing "define expressions"
    (is (= (expand (ast "(define (foo x) x)")) (ast "(define foo (lambda (x) x))")))))
