(ns cloj.parse-test
  (:require [clojure.test :refer :all]
            [cloj.parse :refer :all]))

(deftest parse-test
  (testing "parsing numbers"
    (is (= (parse "123") [[:number 123]]))
    (is (= (parse "123 456") [[:number 123] [:number 456]]))
    )

  (testing "parsing expressions"
    (is (= (parse "(123 4 5 6)") [(list [:number 123] [:number 4] [:number 5] [:number 6])]))
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
