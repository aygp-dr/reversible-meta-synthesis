(ns reversible-meta-synthesis.examples.interop-client
  (:require [reversible-meta-synthesis.interop-bridge :as bridge]
            [reversible-meta-synthesis.reversible-interpreter :as ri]
            [reversible-meta-synthesis.ebg :as ebg]
            [clojure.pprint :as pp]))

;; Example of executing a program in Scheme
(defn example-execute-in-scheme []
  (println "Example: Execute in Scheme")
  
  ;; Define the append program
  (let [program [['append [] '*l '*l] []]
                [['append ['*x '& '*l1] '*l2 ['*x '& '*l3]] 
                 [['append '*l1 '*l2 '*l3]]]]
        
        ;; Define a query
        query [[['append ['/a '/b] ['/c '/d] '*ans]]]
        
        ;; Export to JSON
        program-json (bridge/export-program program)
        query-json (bridge/export-program query)
        
        ;; Construct input for Scheme implementation
        input {:clauses program-json
               :queries query-json}
        
        ;; Call Scheme implementation
        output (bridge/call-external "scheme" "execute" input)]
    
    ;; Display result
    (println "Result from Scheme implementation:")
    (pp/pprint output)))

;; Example of building an explanation in Hy
(defn example-explanation-in-hy []
  (println "\nExample: Build explanation in Hy")
  
  ;; Define the append program
  (let [program [['append [] '*l '*l] []]
                [['append ['*x '& '*l1] '*l2 ['*x '& '*l3]] 
                 [['append '*l1 '*l2 '*l3]]]]
        
        ;; Define a goal
        goal ['append ['/a] ['/b] ['/a '/b]]
        
        ;; Export to JSON
        program-json (bridge/export-program program)
        goal-json (bridge/term->json goal)
        
        ;; Construct input for Hy implementation
        input {:goal goal-json
               :clauses program-json}
        
        ;; Call Hy implementation
        output (bridge/call-external "hy" "build_explanation" input)
        
        ;; Import the explanation
        explanation (bridge/import-explanation output)]
    
    ;; Display result
    (println "Explanation built by Hy:")
    (write-explanation explanation 0)))

;; Helper function to print an explanation tree
(defn write-explanation [node indent]
  (println (apply str (repeat indent " ")) (:goal node))
  (doseq [child (:children node)]
    (write-explanation child (+ indent 2))))

(defn -main [& args]
  (example-execute-in-scheme)
  (example-explanation-in-hy))
