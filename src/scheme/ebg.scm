#!/usr/bin/env scheme

; Explanation tree structure
(define (make-explanation-node goal children)
  (list 'expl-node goal children))

(define (explanation-node-goal node) (cadr node))
(define (explanation-node-children node) (caddr node))

; Building an explanation
(define (build-explanation goal clauses)
  (let ((matching-clauses (filter 
                            (lambda (clause) 
                              (can-match? (car clause) goal))
                            clauses)))
    (if (null? matching-clauses)
        (make-explanation-node goal '())
        (let* ((clause (car matching-clauses))
               (head (car clause))
               (body (cadr clause)))
          (make-explanation-node 
            goal
            (map (lambda (subgoal) 
                   (build-explanation subgoal clauses))
                 body))))))

(define (can-match? pattern term)
  (cond
    ((variable? pattern) #t)
    ((constant? pattern) 
     (equal? (real-value pattern) (real-value term)))
    ((and (pair? pattern) (pair? term))
     (and (can-match? (car pattern) (car term))
          (can-match? (cdr pattern) (cdr term))))
    (else (equal? pattern term))))

; Generalization
(define (generalize-explanation expl-tree)
  (let generalize-term ((term (explanation-node-goal expl-tree)))
    (cond
      ((constant? term) 
       (string->symbol 
         (string-append "*GEN" 
                        (substring (symbol->string term) 1))))
      ((symbol? term) term)
      ((pair? term)
       (cons (generalize-term (car term))
             (generalize-term (cdr term))))
      (else term))))

; Decomposition based on composability
(define (decompose-explanation expl composability decomp-force)
  (let ((goal (explanation-node-goal expl)))
    (if (and (assoc goal composability)
             (<= (cdr (assoc goal composability)) decomp-force))
        (apply append
               (map (lambda (child) 
                      (decompose-explanation 
                        child composability decomp-force))
                    (explanation-node-children expl)))
        (list expl))))

; Exports
(define exports
  (list (cons 'build-explanation build-explanation)
        (cons 'generalize-explanation generalize-explanation)
        (cons 'decompose-explanation decompose-explanation)))
