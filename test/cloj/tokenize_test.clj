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
      (is (between start end 5))))

  (testing "false cases"
    (let [start 0 end 10]
      (is (not (between start end 999)))
      (is (not (between start end 999))))))

(deftest between-ch-test
  (testing "sanity check")
  (is (between-ch \a \z \a))
  (is (between-ch \a \z \z))
  (is (between-ch \A \Z \A))
  (is (not (between-ch \A \Z \a))))

(deftest symbol?-test
  (testing "true cases"
    (is (symbol-or-digit? \a))
    (is (symbol-or-digit? \A))
    (is (symbol-or-digit? \!))
    (is (symbol-or-digit? \1))
    (is (symbol-or-digit? \^)))
  (is (symbol-or-digit? \.))
  (testing "false cases"
    (is (not (symbol-or-digit? \tab)))
    (is (not (symbol-or-digit? \$)))))

(deftest get-symbol-test
  (testing "sanity checks"
    (is (= (get-symbol (seq "abc")) ['() '(:symbol "abc")]))
    (is (= (get-symbol (seq "abc 123")) ['(\space \1 \2 \3) '(:symbol "abc")]))))

(deftest get-string-test
  (testing "sanity checks"
    (is (= (get-string (seq "\"abc\"")) ['() '(:string "abc")]))))

(deftest get-boolean-test
  (testing "sanity checks"
    (is (= (get-boolean (seq "#t")) [nil '(:boolean true)]))
    (is (= (get-boolean (seq "#f")) [nil '(:boolean false)]))))

(deftest tokenize-test
  (testing "sanity checks"
    (is (= (tokenize "123") ['(:number 123)]))
    (is (= (tokenize "lambda") ['(:symbol "lambda")]))
    (is (= (tokenize "\"hello world\"") ['(:string "hello world")]))
    (is (= (tokenize "(lambda)") ['(:lparen \() '(:symbol "lambda") '(:rparen \))])))

  (testing "numbers sanity"
    (is (= (tokenize "123") ['(:number 123)]))
    (is (= (tokenize "1.23") ['(:number 1.23)]))
    (is (= (tokenize "-1.23") ['(:number -1.23)]))
    (is (= (tokenize "-123") ['(:number -123)])))

  (testing "booleans sanity"
    (is (= (tokenize "#t") ['(:boolean true)]))
    (is (= (tokenize "#f") ['(:boolean false)])))

  (testing "strings sanity"
    (is (= (tokenize "\"\"") ['(:string "")]))
    (is (= (tokenize "\"abc\"") ['(:string "abc")])))
    
  (testing "omitting whitespaces"
    (is (= (tokenize " 123 ") (tokenize "123")))))

(run-tests)