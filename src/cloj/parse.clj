(ns cloj.parse
  (:require [cloj.tokenize :refer :all]))

(use '[clojure.string :only (join split)])

(declare parse-many)
(declare parse1)
(declare unparse)

(defn parse1
  [tokens fn-success fn-failure]
  (if (empty? tokens)
    (fn-success (list) (list))
    (let [[token & rest] tokens
          [kind value] token]
      (cond (= kind :number) (fn-success value rest)
            (= kind :string) (fn-success value rest)
            (= kind :boolean) (fn-success value rest)
            (= kind :symbol) (fn-success (symbol value) rest)
            (= kind :quote) (parse1 rest
                                    (fn [expr even-more-tokens] (fn-success (list 'quote expr) even-more-tokens))
                                    fn-failure)
            (= kind :lparen) (parse-many rest
                                         (fn [exprs even-more-tokens]
                                           (let [[[last-type _] & tail] even-more-tokens]
                                             (if (= last-type :rparen)
                                               (fn-success exprs tail)
                                               (fn-failure))))
                                         (fn []
                                           (if (> (count rest) 0)
                                             (let [[[last-type _] & tail] rest]
                                               (if (= (last-type :rparen))
                                                 (fn-success '() tail)
                                                 (fn-failure)))
                                             (fn-failure))))
            :else (fn-failure)))))

(defn parse-many
  [tokens fn-success fn-failure]
  (if (empty? tokens)
    (fn-success (list) (list))
    (parse1 tokens
            (fn [expr more-tokens]
              (parse-many more-tokens
                          (fn [exprs even-more-tokens] (fn-success (conj exprs expr) even-more-tokens))
                          (fn [] (fn-success (list expr) more-tokens))))
            fn-failure)))

(defn parse
  [str]
  (parse-many (tokenize (seq str))
              (fn [exprs tokens] (if (empty? tokens) exprs (throw (Exception. (format "got extra tokens %s" tokens)))))
              (fn [] (throw (Exception. (format "failed to parse %s" str))))))

(defn unparse
  [exprs]
  (join " " (map str exprs)))
