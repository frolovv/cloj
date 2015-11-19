(ns cloj.expand)

(declare expand)
(declare expand1)

(defn const-ast? [ast] (= (first ast) :const))
(defn var-ast? [ast] (= (first ast) :var))
(defn if-ast? [ast] (= (first ast) :if))
(defn let-ast? [ast] (= (first ast) :let))
(defn let*-ast? [ast] (= (first ast) :let*))
(defn lambda-ast? [ast] (= (first ast) :lambda))
(defn applic-ast? [ast] (= (first ast) :application))
(defn and-ast? [ast] (= (first ast) :and))
(defn or-ast? [ast] (= (first ast) :or))


(defn expand1
  [ast]
  (cond (const-ast? ast) ast
        (var-ast? ast) ast
        (if-ast? ast) (let [[_ if-test if-then if-else] ast]
                        [:if (expand1 if-test) (expand1 if-then) (expand1 if-else)])
        (or-ast? ast) (let [[_ exprs] ast]
                         [:or (map expand1 exprs)])
        (and-ast? ast) (let [[_ exprs] ast]
                         [:and (map expand1 exprs)])
        (let-ast? ast) (let [[_ names values body] ast
                             lambda [:lambda names (expand1 body)]
                             applic [:application lambda (map expand1 values)]]
                         applic)
        (let*-ast? ast) (let [[_ names values body] ast
                              reversedNamesAndValues (reverse (map list names values))]
                         (reduce
                            (fn [body [name value]]
                              (let [lambda [:lambda (list name) body]]
                                [:application lambda (list (expand1 value))]))
                            (expand1 body)
                            reversedNamesAndValues
                          ))
        (lambda-ast? ast) (let [[_ params body] ast]
                            [:lambda params (expand1 body)])
        (applic-ast? ast) (let [[_ operator operands] ast]
                            [:application (expand1 operator) (map expand1 operands)])))



(defn expand
  [asts]
  (map expand1 asts))
