(ns reversible-meta-synthesis.core
  (:require [reversible-meta-synthesis.reversible-interpreter :as ri]
            [reversible-meta-synthesis.ebg :as ebg]
            [reversible-meta-synthesis.composability :as comp])
  (:gen-class))

(defn -main [& args]
  (println "Reversible Meta-Synthesis")
  (println "=======================")
  (println "An implementation of \"Inductive Program Synthesis by Using")
  (println "a Reversible Meta-Interpreter\" by Numao and Shimura")
  (println)
  
  (println "Available commands:")
  (println "  examples  - Run example programs")
  (println "  tests     - Run tests")
  (println "  synthesis - Run program synthesis demos")
  
  (when (seq args)
    (case (first args)
      "examples" (do
                   (require 'reversible-meta-synthesis.examples.append)
                   (apply (resolve 'reversible-meta-synthesis.examples.append/-main) (rest args)))
      "tests" (do
                (require 'reversible-meta-synthesis.reversible-interpreter-test)
                (apply (resolve 'reversible-meta-synthesis.reversible-interpreter-test/-main) (rest args)))
      "synthesis" (do
                    (require 'reversible-meta-synthesis.examples.app3-merge3)
                    (apply (resolve 'reversible-meta-synthesis.examples.app3-merge3/-main) (rest args)))
      (println "Unknown command:" (first args)))))
