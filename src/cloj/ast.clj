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

;; (and e1 e2 .. eN)
(defn and-expr?
  [expr]
  (and (list? expr)
       (= (first expr) 'and)))

;; (or e1 e2 .. eN)
(defn or-expr?
  [expr]
  (and (list? expr)
       (= (first expr) 'or)))

;; (lambda (a b c) body)
(defn lambda-expr?
  [expr]
  (and (list? expr)
       (= (first expr) 'lambda)
       (list? (second expr))
       (= (count expr) 3)
  ))

;; (let ((n1 e1) ... (nN eN)) body)
(defn let-expr? [expr]
  (and (list? expr)
       (= (first expr) 'let)
       (list? (second expr))
       (= (count expr) 3)
       ))

;; (let* ((n1 e1) ... (nN eN)) body)
(defn let*-expr? [expr]
  (and (list? expr)
       (= (first expr) 'let*)
       (list? (second expr))
       (= (count expr) 3)
       ))

(defn ast1
  [expr]
  (cond (number? expr) [:const expr]
        (string? expr) [:const expr]
        (symbol? expr) [:var expr]
        (if-expr? expr) (let [[_ if-test if-then if-else] expr]
                          [:if (ast1 if-test) (ast1 if-then) (ast1 if-else)])
        (and-expr? expr) [:and (map ast1 (rest expr))]
        (or-expr? expr) [:or (map ast1 (rest expr))]
        (lambda-expr? expr) (let [[_ params body] expr]
                              [:lambda params (ast1 body)])
        (let-expr? expr) (let [[_ bindings body] expr
                               names (map first bindings)
                               exprs (map second bindings)]
                           [:let names (map ast1 exprs) (ast1 body)])
        (let*-expr? expr) (let [[_ bindings body] expr
                               names (map first bindings)
                               exprs (map second bindings)]
                           [:let* names (map ast1 exprs) (ast1 body)])
        (list? expr) (let [[arg & args] expr
                           operator (ast1 arg)
                           operands (map ast1 args)]
                       [:application operator operands])))
(defn ast
  [str]
  (let [exprs (parse str)
        results (map ast1 exprs)]
    results))
