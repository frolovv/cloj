(ns cloj.eval
  (:require [cloj.ast :refer :all]
            [cloj.expand :refer :all]))

(declare my-eval)
(declare eval1)
(def const-value second)
(def var-name second)

(def GLOBAL-ENV-ATOM (atom (hash-map
                            '+ +
                            '- -
                            'zero? zero?
                            '* *
                            '= =
                            '> >
                            '< <
                            'list list
                            'list? list?
                            'length count
                            'car first
                            'cdr rest
                            'cons cons)))

(defn GLOBAL-ENV [sym] (@GLOBAL-ENV-ATOM sym))

(defn UPDATE-GLOBAL-ENV!
  [sym value]
  (swap! GLOBAL-ENV-ATOM assoc sym value))

(defn make-env
  [old-env names values]
  (let [new-env (zipmap names values)]
    (fn [sym]
      (if (contains? new-env sym)
        (new-env sym)
        (old-env sym)))))

(defn eval1
  [ast env]
  (cond (const-ast? ast) (const-value ast)
        (var-ast? ast) (let [var-value (env (var-name ast))]
                         (if (nil? var-value)
                           (throw (Exception. (str "unbounded variable " (var-name ast))))
                           var-value))
        (if-ast? ast) (let [[_ if-test if-then if-else] ast]
                        (if (eval1 if-test env) (eval1 if-then env) (eval1 if-else env)))
        (define-var-ast? ast) (let [[_ name value] ast]
                                (UPDATE-GLOBAL-ENV! name (eval1 value env))
                                nil)
        (and-ast? ast) (let [[_ expressions] ast]
                         (reduce (fn [result expr] (and result (eval1 expr env))) true expressions))
        (or-ast? ast) (let [[_ expressions] ast]
                        (reduce (fn [result expr] (or result (eval1 expr env))) false expressions))
        (lambda-ast? ast) (let [[_ params body] ast]
                            (fn [& values] (eval1 body (make-env env params values))))
        (applic-ast? ast) (let [[_ operator-ast operands-ast] ast
                                operator (eval1 operator-ast env)
                                operands (map #(eval1 %1 env) operands-ast)]
                            (assert (not (nil? operator)) (format "variable %s is unbound" (var-name operator-ast)))
                            (apply operator operands))))

(defn my-eval
  [str]
  (let [asts (ast str)
        expanded (expand asts)
        results (map #(eval1 %1 GLOBAL-ENV) expanded)]
    (doall (filter #(not (nil? %1)) results))))

(defn SETUP-GLOBAL-ENV
  []
  (do
    ;; numeric
    (my-eval "(define positive? (lambda (n) (if (> n 0) #t #f)))")
    (my-eval "(define negative? (lambda (n) (if (< n 0) #t #f)))")
    (my-eval "(define odd? (lambda (n) (if (zero? n) #f (even? (- n 1)))))")
    (my-eval "(define even? (lambda (n) (if (zero? n) #t (odd? (- n 1)))))")

    ;; boolean 
    (my-eval "(define (not x) (and (boolean? x) (= x #f)))")
    (my-eval "(define (boolean? x) (or (= x #t) (= x #f)))")

    ;; predicates
    (my-eval "(define null? (lambda (xs) (and (list? xs) (zero? (length xs)))))")

    ;; collection
    (my-eval "(define map (lambda (f xs)
                (if (null? xs) (list)
                    (cons (f (car xs)) (map f (cdr xs))))))")
    (my-eval "(define (foldl f acc xs)
                (if (null? xs) acc
                    (foldl f (f acc (car xs)) (cdr xs))))")
    (my-eval "(define (foldr f end xs)
                (if (null? xs) end
                    (f (car xs) (foldr f end (cdr xs)))))")))

(SETUP-GLOBAL-ENV)