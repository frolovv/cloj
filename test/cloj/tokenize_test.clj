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

(deftest handle-hash-test
  (testing "booleans"
    (is (= (handle-hash (seq "#t")) [nil '(:boolean true)]))
    (is (= (handle-hash (seq "#f")) [nil '(:boolean false)])))
  (testing "special chars"
    (is (= (handle-hash (seq "#\\newline")) '[() (:char \newline)]))
    (is (= (handle-hash (seq "#\\tab")) '[() (:char \tab)]))
    (is (= (handle-hash (seq "#\\space")) '[() (:char \space)])))
  (testing "regular chars"
    (is (= (handle-hash (seq "#\\a")) '[() (:char \a)]))
    (is (= (handle-hash (seq "#\\0")) '[() (:char \0)]))
    (is (= (handle-hash (seq "#\\A")) '[() (:char \A)]))))

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

  (testing "chars sanity"
    (is (= (tokenize "#\\a") ['(:char \a)]))
    (is (= (tokenize "#\\newline") ['(:char \newline)]))
    (is (= (tokenize "#\\space") ['(:char \space)]))
    (is (= (tokenize "#\\tab") ['(:char \tab)]))
    (is (= (tokenize "#\\0") ['(:char \0)])))

  (testing "quote sanity"
    (is (= (tokenize "'123") '[(:quote \') (:number 123)]))
    (is (= (tokenize "'a") '[(:quote \') (:symbol "a")]))
    (is (= (tokenize "'()") '[(:quote \') (:lparen \() (:rparen \))]))
    (is (= (tokenize "'#t") '[(:quote \') (:boolean true)])))

  (testing "strings sanity"
    (is (= (tokenize "\"\"") ['(:string "")]))
    (is (= (tokenize "\"abc\"") ['(:string "abc")])))

  (testing "omitting whitespaces"
    (is (= (tokenize " 123 ") (tokenize "123")))))

(run-tests)