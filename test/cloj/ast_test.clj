(ns cloj.ast-test
  (:require [clojure.test :refer :all]
            [cloj.parse :refer :all]
            [cloj.ast :refer :all]))

(deftest ast-parsing-constants
  (testing "parsing numbers"
    (is (= (ast "123") [[:const 123]]))
    (is (= (ast "123 456") [[:const 123] [:const 456]]))
    )

  (testing "parsing strings"
    (is (= (ast "\"\"") [[:const ""]]))
    (is (= (ast "\"123 456\"") [[:const "123 456"]]))
    )

  (testing "parsing booleans"
    (is (= (ast "#t") [[:const true]]))
    (is (= (ast "#f") [[:const false]]))
    )
  )

(deftest ast-parsing-vars
  (testing "parsing var"
    (is (= (ast "abc") [[:var 'abc]]))
    )
  )

(deftest ast-parsing-if-statments
  (testing "parsing if"
    (is (= (ast "(if 1 2 3)") [[:if [:const 1] [:const 2] [:const 3]]]))
    )
  )

(deftest ast-parsing-applications
  (testing "parsing applications"
    (is (= (ast "(+ 1 2 3)") [[:application [:var '+] '([:const 1] [:const 2] [:const 3])]]))
    )
  )

(deftest ast-parsing-applications
  (testing "parsing lambda expressions"
    (is (= (ast "(lambda (x) x)") [[:lambda '(x) [:var 'x]]]))
  ))

(deftest ast-parsing-let-expressions
  (testing "parsing let expressions"
    (is (= (ast "(let ((x 1)) x)") [[:let ['x] [[:const 1]] [:var 'x]]]))
  ))

(deftest ast-parsing-let*-expressions
  (testing "parsing let expressions"
    (is (= (ast "(let* ((x 1)) x)") [[:let* ['x] [[:const 1]] [:var 'x]]]))
  ))

(deftest ast-parsing-and-expressions
  (testing "parsing and expressions"
    (is (= (ast "(and 1 2 3)") [[:and [[:const 1] [:const 2] [:const 3]]]]))
  ))

(deftest ast-parsing-quoted-expressions
  (testing "parsing quoted expressions"
    (is (= (ast "'123") [[:const 123]]))
    (is (= (ast "'#t") [[:const true]]))
    (is (= (ast "'a") [[:const 'a]]))
    (is (= (ast "'\"abc\"") [[:const "abc"]]))
  ))

(deftest ast-parsing-or-expressions
  (testing "parsing and expressions"
    (is (= (ast "(or 1 2 3)") [[:or [[:const 1] [:const 2] [:const 3]]]]))
  ))

(deftest ast-parsing-define-expressions
  (testing "parsing define expressions"
    (is (= (ast "(define x 10)") [[:define 'x [:const 10] ]]))
    (is (= (ast "(define (foo x) x)") [[:define '(foo x) [:var 'x] ]]))
    (is (= (ast "(define (foo) 10)") [[:define '(foo) [:const 10] ]]))
  ))

(run-tests)