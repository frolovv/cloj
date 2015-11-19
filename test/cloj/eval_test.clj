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
    (is (= (my-eval "(if (if 1 2 3) (if 4 5 6) (if 7 8 9))") '(5)))
    ))

(deftest testing-applications
  (testing "+ tests"
    (is (= (my-eval "(+ 1 2 3)") '(6)))
    (is (= (my-eval "(+ 0)") '(0)))
    ))

(deftest testing-make-env
  (testing "make-env sanity"
    (let [env (make-env GLOBAL-ENV ['a 'b] [123 "abc"])]
      (is (= (env 'a) 123))
      (is (= (env 'b) "abc"))
      (is (= (env 'i-dont-exist) nil))
      )))

(deftest testing-lambdas
  (testing "+ tests"
    (is (= (my-eval "((lambda (x) x) 123)") [123]))
    (is (= (my-eval "((lambda (x y z) x) 1 2 3)") [1]))
    (is (= (my-eval "((lambda (x y z) y) 1 2 3)") [2]))
    (is (= (my-eval "((lambda (x y z) (+ x y z)) 1 2 3)") [6]))
    ))

(deftest testing-let-expressions
  (testing "testing expansion and evaluation of lets"
    (is (= (my-eval "(let ((x 123)) x)") [123]))
    ))

(deftest testing-let*-expressions
  (testing "testing expansion and evaluation of let* special form"
    (is (= (my-eval "(let* ((x 123) (y x)) y)") [123]))
    ))
