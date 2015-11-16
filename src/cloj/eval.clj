(ns cloj.eval
   (:require [cloj.ast :refer :all]))

(declare my-eval)
(declare eval1)

(defn const-ast? [ast] (= (first ast) :const))
(def const-value second)

(defn if-ast? [ast] (= (first ast) :if))
(defn if-test [ast] (nth ast 1))
(defn if-then [ast] (nth ast 2))
(defn if-else [ast] (nth ast 3))

(defn applic-ast?
  [ast]
  (= (first ast) :application))

(defn applic-operator [ast] (nth ast 1))
(defn applic-operands [ast] (nth ast 2))

(defn var-ast? [ast] (= (first ast) :var))

(defn var-name [ast] (second ast))

(def GLOBAL-ENV (hash-map '+ +, '- -))

(defn eval1
  [ast env]
  (cond (const-ast? ast) (const-value ast)
        (if-ast? ast) (if (eval1 (if-test ast) env) (eval1 (if-then ast) env) (eval1 (if-else ast) env))
        (var-ast? ast) (env (var-name ast))
        (applic-ast? ast) (apply (eval1 (applic-operator ast) env) (map #(eval1 %1 env) (applic-operands ast)))
        ))

(defn my-eval
  [str]
  (let [asts (ast str)]
    (map #(eval1 %1 GLOBAL-ENV) asts)))
