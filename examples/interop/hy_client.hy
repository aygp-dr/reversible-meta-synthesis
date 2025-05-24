#!/usr/bin/env hy

(import [src.hy.interop_bridge [
    export-program 
    import-program
    export-explanation
    import-explanation
    call-external
]])

(defn example-execute-in-prolog []
  (print "Example: Execute in Prolog")
  
  ;; Define the append program
  (setv program [
    [["append" [] "*l" "*l"] []]
    [["append" ["*x" "*l1"] "*l2" ["*x" "*l3"]] 
     [["append" "*l1" "*l2" "*l3"]]]
  ])
  
  ;; Define a query
  (setv query [[["append" ["/a" "/b"] ["/c" "/d"] "*ans"]]])
  
  ;; Export to JSON
  (setv program-json (export-program program))
  (setv query-json (export-program query))
  
  ;; Construct input for Prolog implementation
  (setv input {
    "clauses" program-json
    "queries" query-json
  })
  
  ;; Call Prolog implementation
  (setv output (call-external "prolog" "execute" input))
  
  ;; Display result
  (print "Result from Prolog implementation:")
  (print output))

(defn example-explanation-in-scheme []
  (print "\nExample: Build explanation in Scheme")
  
  ;; Define the append program
  (setv program [
    [["append" [] "*l" "*l"] []]
    [["append" ["*x" "*l1"] "*l2" ["*x" "*l3"]] 
     [["append" "*l1" "*l2" "*l3"]]]
  ])
  
  ;; Define a goal
  (setv goal ["append" ["/a"] ["/b"] ["/a" "/b"]])
  
  ;; Export to JSON
  (setv program-json (export-program program))
  (setv goal-json (term-to-json goal))
  
  ;; Construct input for Scheme implementation
  (setv input {
    "goal" goal-json
    "clauses" program-json
  })
  
  ;; Call Scheme implementation
  (setv output (call-external "scheme" "build_explanation" input))
  
  ;; Import the explanation
  (setv explanation (import-explanation output))
  
  ;; Display result
  (print "Explanation built by Scheme:")
  (write-explanation explanation 0))

;; Helper function to print an explanation tree
(defn write-explanation [node indent]
  (print (.join "" (list (* indent " "))) (. node goal))
  (for [child (. node children)]
    (write-explanation child (+ indent 2))))

;; Helper function for term_to_json (simplified for example)
(defn term-to-json [term]
  (cond
    [(string? term) {"type" "string" "value" term}]
    [(symbol? term) {"type" "atom" "value" (name term)}]
    [(list? term) {"type" "list" 
                   "value" (list (map term-to-json term))}]
    [True {"type" "unknown"}]))

(defmain [&rest args]
  (example-execute-in-prolog)
  (example-explanation-in-scheme))
