(ns cloj.expand-test
  (:require [clojure.test :refer :all]
            [cloj.ast :refer :all]
            [cloj.expand :refer :all]))

(deftest expand-test
  (testing "forms that should not be expanded"
    (is (= (ast "123") (expand (ast "123"))))
    (is (= (ast "123 456") (expand (ast "123 456"))))

    (is (= (ast "abc") (expand (ast "abc"))))
    (is (= (ast "(if 1 2 3)") (expand (ast "(if 1 2 3)"))))
    (is (= (ast "(+ 1 2 3)") (expand (ast "(+ 1 2 3)"))))
    (is (= (ast "(lambda (x) x)") (expand (ast "(lambda (x) x)"))))
    ))
