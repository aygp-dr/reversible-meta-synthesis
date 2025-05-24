#!/usr/bin/env scheme
(load "../../src/scheme/reversible-interpreter.scm")

; Example of using the reversible interpreter to execute the append program
(define append-clauses
  '(((append () *l *l) ())
    ((append (*x . *l1) *l2 (*x . *l3)) 
     ((append *l1 *l2 *l3)))))

(define append-queries
  '(((append (/x) (/y) *ans))))

(display "Running append program: ")
(display (prolog append-clauses append-queries))
(newline)

; Example of program synthesis
(display "Synthesizing append program from examples")
(newline)
(display (synthesize-program 
           '(((append (/a) (/b) (/a /b))))
           '()))
(newline)
