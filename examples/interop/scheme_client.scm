#!/usr/bin/env guile
!#

(load "../../src/scheme/interop-bridge.scm")

;; Example of executing a program in Clojure
(define (example-execute-in-clojure)
  (display "Example: Execute in Clojure\n")
  
  ;; Define the append program
  (define program
    '(((append () *l *l) ())
      ((append (*x . *l1) *l2 (*x . *l3)) 
       ((append *l1 *l2 *l3)))))
  
  ;; Define a query
  (define query
    '(((append (/a /b) (/c /d) *ans))))
  
  ;; Export to JSON
  (define program-json (export-program program))
  (define query-json (export-program query))
  
  ;; Construct input for Clojure implementation
  (define input
    `(("clauses" . ,program-json)
      ("queries" . ,query-json)))
  
  ;; Call Clojure implementation
  (define output (call-external "clojure" "execute" input))
  
  ;; Display result
  (display "Result from Clojure implementation:\n")
  (display output)
  (newline))

;; Example of building an explanation in Prolog
(define (example-explanation-in-prolog)
  (display "\nExample: Build explanation in Prolog\n")
  
  ;; Define the append program
  (define program
    '(((append () *l *l) ())
      ((append (*x . *l1) *l2 (*x . *l3)) 
       ((append *l1 *l2 *l3)))))
  
  ;; Define a goal
  (define goal '(append (/a) (/b) (/a /b)))
  
  ;; Export to JSON
  (define program-json (export-program program))
  (define goal-json (term->json goal))
  
  ;; Construct input for Prolog implementation
  (define input
    `(("goal" . ,goal-json)
      ("clauses" . ,program-json)))
  
  ;; Call Prolog implementation
  (define output (call-external "prolog" "build_explanation" input))
  
  ;; Import the explanation
  (define explanation (import-explanation output))
  
  ;; Display result
  (display "Explanation built by Prolog:\n")
  (write-explanation explanation 0))

;; Helper function to print an explanation tree
(define (write-explanation node indent)
  (display (make-string indent #\space))
  (display (cadr node))
  (newline)
  (for-each (lambda (child)
              (write-explanation child (+ indent 2)))
            (caddr node)))

;; Run examples
(example-execute-in-clojure)
(example-explanation-in-prolog)
