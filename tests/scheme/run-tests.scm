#!/usr/bin/env guile
!#

(load "../../src/scheme/reversible-interpreter.scm")
(load "../../src/scheme/ebg.scm")

;; Simple test framework
(define (assert-equal expected actual message)
  (if (equal? expected actual)
      (begin
        (display "PASS: ")
        (display message)
        (newline))
      (begin
        (display "FAIL: ")
        (display message)
        (newline)
        (display "  Expected: ")
        (display expected)
        (newline)
        (display "  Actual:   ")
        (display actual)
        (newline))))

;; Test cases for the reversible interpreter
(define (test-reversible-interpreter)
  (display "\nTesting Reversible Interpreter:\n")
  
  ;; Test append execution
  (let* ((append-clauses
           '(((append () *l *l) ())
             ((append (*x . *l1) *l2 (*x . *l3)) 
              ((append *l1 *l2 *l3)))))
         (append-queries
           '(((append (/a /b) (/c /d) *ans))))
         (results (prolog append-clauses append-queries)))
    
    (assert-equal 
      '(((*ans /a /b /c /d)))
      results
      "Basic append execution")))

;; Test cases for EBG
(define (test-ebg)
  (display "\nTesting Explanation-Based Learning:\n")
  
  ;; Test explanation building
  (let* ((append-clauses
           '(((append () *l *l) ())
             ((append (*x . *l1) *l2 (*x . *l3)) 
              ((append *l1 *l2 *l3)))))
         (goal '(append (/a) (/b) (/a /b)))
         (explanation (build-explanation goal append-clauses)))
    
    (assert-equal 
      'expl-node
      (car explanation)
      "Building explanation tree")
    
    ;; Test explanation generalization
    (let ((generalized (generalize-explanation explanation)))
      (assert-equal 
        'expl-node
        (car generalized)
        "Generalizing explanation tree"))))

;; Run all tests
(define (run-all-tests)
  (test-reversible-interpreter)
  (test-ebg)
  (display "\nAll tests completed.\n"))

(run-all-tests)
