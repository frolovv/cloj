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

(defn symbol?
  [ch]
    (or (between-ch \a \z ch)
        (between-ch \A \Z ch)
        (contains? valid-symbols ch)))

(defn get-digits
  [chars]
  (let [[digits rest] (split-with digit? chars)
        joined (clojure.string/join digits)
        num (read-string joined)
        ]
    (list rest (list :number num))))

(defn get-symbol
  [chars]
  (let [[symbols rest] (split-with symbol? chars)
        joined (clojure.string/join symbols)
        ]
    (list rest (list :symbol joined))))

(defn get-string
  [chars]
  (let [[chars' more-chars] (split-with #(not (= %1 \")) (rest chars))
        joined (clojure.string/join chars')]
    (list (rest more-chars) (list :string joined))))

(defn tokenize1
  [chars tokens]
  (if (empty? chars)
    tokens
    (let [[ch & rest] chars]
      (cond (whitespace? ch) (recur rest tokens)
            (= ch \() (recur rest (conj tokens (list :lparen ch)))
            (= ch \)) (recur rest (conj tokens (list :rparen ch)))
            (digit? ch) (let [[rest' token] (get-digits chars)]
                          (recur rest' (conj tokens token)))
            (symbol? ch) (let [[rest' token] (get-symbol chars)]
                           (recur rest' (conj tokens token)))
            (= ch \") (let [[rest' token] (get-string chars)]
                        (recur rest' (conj tokens token)))
            :else (throw (Exception. (format "unknown char [%s]" ch)))
            )
      )))


(defn tokenize
  [str]
  (tokenize1 (seq str) []))
