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
