(ns cloj.ast
   (:require [cloj.parse :refer :all]))

(declare ast1)
(declare ast)

(defn const?
  [expr]
  (or (number? expr) (string? expr)))

;; (if test then else)
(defn if-expr?
  [expr]
  (and (list? expr)
       (= (count expr) 4)
       (= (first expr) 'if)))

(defn ast1
  [expr]
  (cond (number? expr) [:const expr]
        (string? expr) [:const expr]
        (symbol? expr) [:var expr]
        (if-expr? expr) (let [[_ if-test if-then if-else] expr]
                          [:if (ast1 if-test) (ast1 if-then) (ast1 if-else)])
        (list? expr) (let [[arg & args] expr
                           operator (ast1 arg)
                           operands (map ast1 args)]
                       [:application operator operands])))
(defn ast
  [str]
  (let [exprs (parse str)
        results (map ast1 exprs)]
    results))
