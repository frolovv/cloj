(ns cloj.core
  (:require [cloj.eval :refer :all]))

(defn -main
  []
  (println "cloj 0.1")
  (loop []
    (let [line (read-line)]
      (when (not (= line "quit"))
        (let [results (try (my-eval line)
                        (catch Exception e (.getMessage e)))]
          (println results)
          (recur)
          )))))