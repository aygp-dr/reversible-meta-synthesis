#!/usr/bin/guile
!#
#|
Reversible Meta-Interpreter in Scheme
Based on the Prolog implementation from "Inductive Program Synthesis by Using 
a Reversible Meta-Interpreter" by Numao and Shimura
|#

; Environment handling
(define (make-env) '())
(define (extend-env var val env) (cons (cons var val) env))
(define (lookup-var var env)
  (let ((pair (assoc var env)))
    (if pair (cdr pair) #f)))

; Predicate utilities
(define (variable? x)
  (and (symbol? x) (char=? (string-ref (symbol->string x) 0) #\*)))

(define (constant? x)
  (and (symbol? x) (char=? (string-ref (symbol->string x) 0) #\/)))

(define (real-value x)
  (if (constant? x)
      (string->symbol (substring (symbol->string x) 1))
      x))

; The reversible interpreter
(define (prolog clauses queries)
  (map (lambda (query) (goalseq query (make-env) clauses)) queries))

(define (goalseq goals env clauses)
  (if (null? goals)
      env
      (let ((new-env (goal (car goals) env clauses)))
        (if new-env
            (goalseq (cdr goals) new-env clauses)
            #f))))

(define (goal g env clauses)
  (let loop ((cs clauses))
    (if (null? cs)
        #f
        (let* ((clause (car cs))
               (head (car clause))
               (body (cadr clause))
               (new-env (match-head head g env)))
          (if new-env
              (goalseq body new-env clauses)
              (loop (cdr cs)))))))

(define (match-head pattern term env)
  (cond
    ((variable? pattern)
     (let ((val (lookup-var pattern env)))
       (if val
           (if (equal? val term) env #f)
           (extend-env pattern term env))))
    ((constant? pattern)
     (if (equal? (real-value pattern) term) env #f))
    ((and (pair? pattern) (pair? term))
     (let ((new-env (match-head (car pattern) (car term) env)))
       (if new-env
           (match-head (cdr pattern) (cdr term) new-env)
           #f)))
    ((equal? pattern term) env)
    (else #f)))

; Synthesis mode
(define (synthesize-program queries results)
  (display "Program synthesis from examples"))

; Exports
(define exports
  (list (cons 'prolog prolog)
        (cons 'synthesize-program synthesize-program)))
