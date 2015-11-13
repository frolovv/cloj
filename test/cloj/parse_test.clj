(ns cloj.parse-test
  (:require [clojure.test :refer :all]
            [cloj.parse :refer :all]))

(deftest parse-test
  (testing "parsing numbers"
    (is (= (parse "123") [123]))
    (is (= (parse "123 456") [123 456]))
    )

  (testing "parsing symbols"
    (is (= (parse "abc") ['abc]))
    )

  (testing "parsing strings"
    (is (= (parse "\"abc\"") ["abc"]))
    )

  (testing "parsing expressions"
    (is (= (parse "(123 4 5 6)") [(list 123 4 5 6)]))
    (is (= (parse "(1 (2 (3)))") ['(1 (2 (3)))]))
    (is (thrown? Exception (parse "()")))
    )
  )

(defn parse-unparse
  [str]
  (= (unparse (parse str)) str))

(deftest parse-unparse-test
  (testing "using unparse function"
    (is (parse-unparse "123"))
    (is (parse-unparse "123 456"))
    (is (parse-unparse "(123)"))
    ))

(run-tests)
