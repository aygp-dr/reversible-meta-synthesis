#!/usr/bin/env guile
!#

(load "../../src/scheme/interop-bridge.scm")

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

;; Test export and import of programs
(define (test-export-import-program)
  (let* ((program
           '(((append () *l *l) ())
             ((append (*x . *l1) *l2 (*x . *l3)) 
              ((append *l1 *l2 *l3)))))
         
         ;; Export to JSON
         (json-data (export-program program))
         
         ;; Import back
         (imported-program (import-program json-data)))
    
    ;; Should match the original
    (assert-equal program imported-program
                  "Programs should match after export/import cycle")))

;; Test export and import of explanation trees
(define (test-export-import-explanation)
  (let* ((explanation
           (list 'expl-node
                 '(append (/a) (/b) (/a /b))
                 (list (list 'expl-node
                             '(append () (/b) (/b))
                             '()))))
         
         ;; Export to JSON
         (json-data (export-explanation explanation))
         
         ;; Import back
         (imported-explanation (import-explanation json-data)))
    
    ;; Should match the original
    (assert-equal explanation imported-explanation
                  "Explanation trees should match after export/import cycle")))

;; Run all tests
(define (run-all-tests)
  (display "\nRunning Interop Bridge Tests:\n")
  (test-export-import-program)
  (test-export-import-explanation)
  (display "\nAll tests completed.\n"))

(run-all-tests)
