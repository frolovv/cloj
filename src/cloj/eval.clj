(ns cloj.eval
   (:require [cloj.ast :refer :all]))

(declare my-eval)
(declare eval1)

(defn const-ast?
  [ast]
  (= (first ast) :const))

(def const-value second)

(defn if-ast?
  [ast]
  (= (first ast) :if))

(defn if-test [ast] (nth ast 1))
(defn if-then [ast] (nth ast 2))
(defn if-else [ast] (nth ast 3))

(defn eval1
  [ast]
  (cond (const-ast? ast) (const-value ast)
        (if-ast? ast) (if (eval1 (if-test ast)) (eval1 (if-then ast)) (eval1 (if-else ast)))))

(defn my-eval
  [str]
  (let [asts (ast str)]
    (map eval1 asts)))
