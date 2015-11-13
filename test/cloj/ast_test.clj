(ns cloj.ast-test
  (:require [clojure.test :refer :all]
            [cloj.parse :refer :all]
            [cloj.ast :refer :all]))

(deftest ast-test
  (testing "parsing if-constants"
    (is (= (ast "123") [:const 123]))
    (is (= (ast "123 456") (list [:const 123] [:const 456])))
    )
  )
