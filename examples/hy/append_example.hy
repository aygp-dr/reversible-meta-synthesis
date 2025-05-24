#!/usr/bin/env hy

(import [src.hy.reversible_interpreter [reverse-interpreter]])

(defn append-example []
  (print "Append Example (Execution mode):")
  (setv clauses 
    [[["append" [] "*l" "*l"] []]
     [["append" ["*x" "*l1"] "*l2" ["*x" "*l3"]] 
      [["append" "*l1" "*l2" "*l3"]]]])
     
  (setv queries [[["append" ["/x"] ["/y"] "*ans"]]])
  
  (setv results (reverse-interpreter clauses queries))
  (print "Results:" results))

(defn synthesize-append-example []
  (print "\nAppend Example (Synthesis mode):")
  ; In a real implementation, this would use the reversible interpreter in synthesis mode
  (print "Synthesized program that would append [/a] and [/b] to get [/a /b]:"))

(defmain [&rest args]
  (append-example)
  (synthesize-append-example))
