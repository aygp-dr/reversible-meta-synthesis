(ns reversible-meta-synthesis.examples.append
  (:require [clojure.spec.alpha :as s]
            [reversible-meta-synthesis.reversible-interpreter :as ri]))

;; Define the append program using proper Clojure data structures
(def append-clauses
  [[['append [] :*l :*l] []]
   [['append [:*x :*l1] :*l2 [:*x :*l3]]
    [['append :*l1 :*l2 :*l3]]]])

(defn example-execution []
  (println "Running append program in execution mode:")

  (let [queries [['append [:a :b] [:c :d] :*ans]]
        results (ri/prolog append-clauses [queries])]

    (println "Query:" queries)
    (println "Result:" (first results))))

(s/fdef example-execution
  :args (s/cat)
  :ret nil?)

(defn example-synthesis []
  (println "\nSynthesizing append program from examples:")

  (let [examples [['append [] [:a :b] [:a :b]]
                  ['append [:c] [:d] [:c :d]]]
        synthesized (ri/synthesize examples)]

    (println "Examples:" examples)
    (println "Synthesized program:" synthesized)))

(s/fdef example-synthesis
  :args (s/cat)
  :ret nil?)

(defn -main [& args]
  (example-execution)
  (example-synthesis))

(s/fdef -main
  :args (s/* string?))
