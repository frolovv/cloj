(ns cloj.eval
  (:require [cloj.ast :refer :all]
            [cloj.expand :refer :all]))

(declare my-eval)
(declare eval1)
(def const-value second)
(def var-name second)

(def GLOBAL-ENV (hash-map '+ +, '- -))

(defn make-env
  [old-env names values]
  (let [new-env (zipmap names values)]
    (fn [sym] (or (new-env sym) (old-env sym)))))


(defn eval1
  [ast env]
  (cond (const-ast? ast) (const-value ast)
        (var-ast? ast) (env (var-name ast))
        (if-ast? ast) (let [[_ if-test if-then if-else] ast]
                        (if (eval1 if-test env) (eval1 if-then env) (eval1 if-else env)))
        (and-ast? ast) (let [[_ expressions] ast]
                         (reduce (fn [result expr] (and result (eval1 expr env))) true expressions))
        (or-ast? ast) (let [[_ expressions] ast]
                        (reduce (fn [result expr] (or result (eval1 expr env))) false expressions))
        (lambda-ast? ast) (let [[_ params body] ast]
                            (fn [& values] (eval1 body (make-env env params values))))
        (applic-ast? ast) (let [[_ operator operands] ast]
                            (apply (eval1 operator env) (map #(eval1 %1 env) operands)))
        ))

(defn my-eval
  [str]
  (let [asts (ast str)
        expanded (expand asts)]
    (map #(eval1 %1 GLOBAL-ENV) expanded)))
