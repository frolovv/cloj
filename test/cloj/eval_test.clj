(ns cloj.eval-test
  (:require [clojure.test :refer :all]
            [cloj.eval :refer :all]))

(deftest testing-constants
  (testing "constants"
    (is (= (my-eval "123") '(123)))
    (is (= (my-eval "1 2 3") '(1 2 3)))
    (is (= (my-eval "\"abc\"") '("abc")))
    ))

(deftest testing-vars
  (testing "vars"
    (is (= (my-eval "zero?") (list zero?)))
    (is (= (my-eval "+") (list +)))
    (is (thrown? Exception (my-eval "xyz")))
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

(deftest testing-and-expressions
  (testing "testing evaluation of and expression"
    (is (= (my-eval "(and 1 2 3)") [3]))
    (is (= (my-eval "(and)") [true]))
    (is (= (my-eval "(and #t)") [true]))
    (is (= (my-eval "(and #t #f)") [false]))
    ))

(deftest testing-or-expressions
  (testing "testing evaluation of or expression"
    (is (= (my-eval "(or 1 2 3)") [1]))
    (is (= (my-eval "(or)") [false]))
    (is (= (my-eval "(or #t #f)") [true]))
    ))

(deftest testing-define-expressions
  (testing "testing evaluation of define expression"
    (is (= (my-eval "(define x 10) x") [10]))
    (is (= (my-eval "(define x (lambda (x) (+ x 100))) (x 100)") [200]))
    (is (= (my-eval "(define (foo x) (+ x 100)) (x 100)") [200]))
    )

  (testing "testing evaluation of complex define expressions"
    (is (=
          (my-eval "(define fact
                   (lambda (n)
                   (if (zero? n)
                   1
                   (* n (fact (- n 1))))))

                   (fact 5)")
          [120]))
    )

  (testing "testing evaluation of mutually recursive functions"
    (is (=
          (my-eval "(define odd?
                   (lambda (n)
                   (if (zero? n) #f
                   (even? (- n 1)))))

                   (define even?
                   (lambda (n)
                   (if (zero? n) #t
                   (odd? (- n 1)))))

                   (odd? 0)
                   (odd? 1)
                   (even? 0)
                   (even? 1)
                   ")
          [false true true false]))
    )

  (testing "testing evaluation of higher order functions"
    (is (=
          (my-eval "(define make-add-n
                   (lambda (n)
                   (lambda(x) (+ n x))
                   ))

                   (define add-one (make-add-n 1))
                   (add-one 0)
                   (add-one 1)
                   (add-one 99)
                   ")
          [1 2 100]))
    )
  )

  (run-tests)