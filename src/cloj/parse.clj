(ns cloj.parse
  (:require [cloj.tokenize :refer :all]))

(use '[clojure.string :only (join split)])

(declare parse-many)
(declare parse1)
(declare unparse)

(defn parse1
  [tokens succ fail]
  (if (empty? tokens)
    (succ (list) (list))
    (let [[token & rest] tokens
          [kind value] token]
      (cond (= kind :number) (succ value rest)
            (= kind :string) (succ value rest)
            (= kind :boolean) (succ value rest)
            (= kind :symbol) (succ (symbol value) rest)
            (= kind :lparen) (parse-many rest
                                         (fn [exprs even-more-tokens]
                                           (let [[[last-type _] & tail] even-more-tokens]
                                             (if (= last-type :rparen)
                                               (succ exprs tail)
                                               (fail))))
                                         fail)
            :else (fail))

      )
    ))


(defn parse-many
  [tokens succ fail]
  (if (empty? tokens)
    (succ (list) (list))
    (parse1 tokens
            (fn [expr more-tokens]
              (parse-many more-tokens
                          (fn [exprs even-more-tokens] (succ (conj exprs expr) even-more-tokens))
                          (fn [] (succ (list expr) more-tokens))))
            fail)))

(defn parse
  [str]
  (parse-many (tokenize (seq str))
              (fn [exprs tokens] (if (empty? tokens) exprs (throw (Exception. (format "got extra tokens %s" tokens)))))
              (fn [] (throw (Exception. (format "failed to parse %s" str))))))

(defn unparse
  [exprs]
  (join " " (map str exprs)))
