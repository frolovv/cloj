(ns cloj.eval
   (:require [cloj.ast :refer :all]))

(declare my-eval)
(declare eval1)

(defn const-ast? [ast] (= (first ast) :const))
(def const-value second)

(defn if-ast? [ast] (= (first ast) :if))
(defn applic-ast? [ast] (= (first ast) :application))
(defn var-ast? [ast] (= (first ast) :var))
(def var-name second)

(defn lambda-ast? [ast] (= (first ast) :lambda))

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
        (lambda-ast? ast) (let [[_ params body] ast]
                            (fn [& values] (eval1 body (make-env env params values))))
        (applic-ast? ast) (let [[_ operator operands] ast]
                            (apply (eval1 operator env) (map #(eval1 %1 env) operands)))
        ))

(defn my-eval
  [str]
  (let [asts (ast str)]
    (map #(eval1 %1 GLOBAL-ENV) asts)))
