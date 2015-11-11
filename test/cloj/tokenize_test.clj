(ns cloj.tokenize-test
  (:require [clojure.test :refer :all]
            [cloj.tokenize :refer :all]))

(deftest digit-test
  (testing "is digit? test"
    (is (digit? \1))
    (is (digit? \0))
    (is (digit? \9))
    (is (not (digit? \a)))))

(deftest whitespace-test
  (testing "whitespace?"
    (is (whitespace? \space))
    (is (whitespace? \tab))
    (is (whitespace? \newline))))

(deftest between-test
  (testing "true cases"
    (let [start 0 end 10]
      (is (between start end 0))
      (is (between start end 10))
      (is (between start end 5))
      ))

  (testing "false cases"
    (let [start 0 end 10]
      (is (not (between start end 999)))
      (is (not (between start end 999)))
      ))
)

(deftest between-ch-test
  (testing "sanity check")
    (is (between-ch \a \z \a))
    (is (between-ch \a \z \z))
    (is (between-ch \A \Z \A))
    (is (not (between-ch \A \Z \a)))
)

(deftest get-digits-test
  (testing "getting chars from char list"
    (is (= (vector (list \space \4 \5 \6) '(:number 123)) (get-digits (seq "123 456"))))
    (is (= (vector '() '(:number 123)) (get-digits (seq "123"))))

    ))

(deftest symbol?-test
  (testing "true cases"
    (is (symbol? \a))
    (is (symbol? \A))
    (is (symbol? \!))
    (is (symbol? \^))
    )
  (testing "false cases"
    (is (not (symbol? \1)))
    (is (not (symbol? \tab)))
    (is (not (symbol? \.)))
    (is (not (symbol? \$)))
    )
  )

(deftest get-symbol-test
  (testing "sanity checks"
    (is (= (get-symbol (seq "abc")) ['() '(:symbol "abc")]))
    (is (= (get-symbol (seq "abc 123")) ['(\space \1 \2 \3) '(:symbol "abc")]))
    ))

(deftest get-string-test
  (testing "sanity checks"
    (is (= (get-string (seq "\"abc\"")) ['() '(:string "abc")]))
    ))

(deftest tokenize-test
  (testing "sanity checks"
    (is (= (tokenize "123") ['(:number 123)]))
    (is (= (tokenize "lambda") ['(:symbol "lambda")]))
    (is (= (tokenize "\"hello world\"") ['(:string "hello world")]))
    (is (= (tokenize "(lambda)") ['(:lparen \() '(:symbol "lambda") '(:rparen \))]))
    )
  (testing "omitting whitespaces"
    (is (= (tokenize " 123 ") (tokenize "123")))
    )
  )

(run-tests)
