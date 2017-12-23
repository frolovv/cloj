(ns cloj.tokenize)

(declare tokenize)
(declare tokenize1)

(defn between
  [start end n]
  (and (>= n start) (<= n end)))

(def valid-symbols (set (seq "!'*+-/<>=?^")))

(defn between-ch
  [start-ch end-ch ch]
  (between (int start-ch) (int end-ch) (int ch)))

(def digit? (partial between-ch \0 \9))

(defn whitespace?
  [ch]
  (between 0 32 (int ch)))

(defn symbol-or-digit?
  [ch]
  (or (between-ch \a \z ch)
      (between-ch \A \Z ch)
      (digit? ch)
      (= ch \.)
      (contains? valid-symbols ch)))

(defn get-symbol
  [chars]
  (let [[symbols rest] (split-with symbol-or-digit? chars)
        joined (clojure.string/join symbols)
        value (read-string joined)
        token (if (number? value)
                (list :number value)
                (list :symbol (name value)))]
    (list rest token)))

(defn get-string
  [chars]
  (let [[chars' more-chars] (split-with #(not (= %1 \")) (rest chars))
        joined (clojure.string/join chars')]
    (list (rest more-chars) (list :string joined))))

(defn fail-fast [ch] (throw (Exception. (format "unknown char [%s]" ch))))

(defn handle-hash
  [chars]
  (let [[__hash ch & rest] chars]
    (cond
      (= \t ch) (list rest '(:boolean true))
      (= \f ch) (list rest '(:boolean false))
      (= \\ ch) (let [[symbols rest'] (split-with symbol-or-digit? rest)
                      value (clojure.string/join symbols)]
                  (cond
                    (= "newline" value) (list rest' '(:char \newline))
                    (= "tab" value) (list rest' '(:char \tab))
                    (= "space" value) (list rest' '(:char \space))
                    (= (count value) 1) (list rest' (list :char (first symbols)))
                    :else (throw (Exception. (format "can't handle char [%s]" chars)))))
      :else (throw (Exception. (format "can't handle char [%s]" chars))))))

(defn tokenize1
  [chars tokens]
  (if (empty? chars)
    tokens
    (let [[ch & rest] chars]
      (cond (whitespace? ch) (recur rest tokens)
            (= ch \() (recur rest (conj tokens (list :lparen ch)))
            (= ch \)) (recur rest (conj tokens (list :rparen ch)))
            (= ch \#) (let [[rest' token] (handle-hash chars)]
                        (recur rest' (conj tokens token)))
            (symbol-or-digit? ch) (let [[rest' token] (get-symbol chars)]
                                    (recur rest' (conj tokens token)))
            (= ch \") (let [[rest' token] (get-string chars)]
                        (recur rest' (conj tokens token)))
            :else (throw (Exception. (format "unknown char [%s]" ch)))))))

(defn tokenize
  [str]
  (tokenize1 (seq str) []))
