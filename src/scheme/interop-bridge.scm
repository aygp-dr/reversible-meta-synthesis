#!/usr/bin/env scheme

;; Load required libraries
(use-modules (web client)
             (web response)
             (web uri)
             (json))

;; Convert a program to JSON
(define (export-program clauses)
  (map clause->json clauses))

;; Convert JSON back to a program
(define (import-program json-data)
  (map json->clause json-data))

;; Convert a clause to JSON
(define (clause->json clause)
  (let ((head (car clause))
        (body (cadr clause)))
    `(("head" . ,(term->json head))
      ("body" . ,(map term->json body)))))

;; Convert JSON back to a clause
(define (json->clause json-clause)
  (let ((head (assoc-ref json-clause "head"))
        (body (assoc-ref json-clause "body")))
    (list (json->term head)
          (map json->term body))))

;; Convert a term to JSON
(define (term->json term)
  (cond
    ((variable? term)
     `(("type" . "var")
       ("name" . ,(symbol->string term))))
    ((constant? term)
     `(("type" . "const")
       ("name" . ,(substring (symbol->string term) 1))))
    ((symbol? term)
     `(("type" . "atom")
       ("value" . ,(symbol->string term))))
    ((number? term)
     `(("type" . "number")
       ("value" . ,term)))
    ((string? term)
     `(("type" . "string")
       ("value" . ,term)))
    ((list? term)
     `(("type" . "list")
       ("value" . ,(map term->json term))))
    (else
     `(("type" . "unknown")))))

;; Convert JSON back to a term
(define (json->term json-term)
  (let ((type (assoc-ref json-term "type")))
    (cond
      ((string=? type "var")
       (string->symbol 
         (string-append "*" (assoc-ref json-term "name"))))
      ((string=? type "const")
       (string->symbol 
         (string-append "/" (assoc-ref json-term "name"))))
      ((string=? type "atom")
       (string->symbol (assoc-ref json-term "value")))
      ((string=? type "number")
       (assoc-ref json-term "value"))
      ((string=? type "string")
       (assoc-ref json-term "value"))
      ((string=? type "list")
       (map json->term (assoc-ref json-term "value")))
      (else #f))))

;; Helper predicates
(define (variable? x)
  (and (symbol? x) 
       (let ((s (symbol->string x)))
         (and (> (string-length s) 0)
              (char=? (string-ref s 0) #\*)))))

(define (constant? x)
  (and (symbol? x) 
       (let ((s (symbol->string x)))
         (and (> (string-length s) 0)
              (char=? (string-ref s 0) #\/)))))

;; Convert an explanation tree to JSON
(define (export-explanation expl)
  (explanation->json expl))

;; Convert JSON back to an explanation tree
(define (import-explanation json-data)
  (json->explanation json-data))

;; Convert explanation tree to JSON
(define (explanation->json expl)
  (let ((goal (cadr expl))
        (children (caddr expl)))
    `(("goal" . ,(term->json goal))
      ("children" . ,(map explanation->json children)))))

;; Convert JSON back to explanation tree
(define (json->explanation json-expl)
  (let ((goal (assoc-ref json-expl "goal"))
        (children (assoc-ref json-expl "children")))
    (list 'expl-node 
          (json->term goal)
          (map json->explanation children))))

;; Call external implementation via HTTP
(define (call-external language function input)
  (let* ((uri (build-uri 'http
                         #:host "localhost"
                         #:port 8080
                         #:path (string-append "/api/" 
                                               language "/" 
                                               function)))
         (json-data (scm->json-string input))
         (response (http-post uri #:body json-data)))
    (if (= (response-code response) 200)
        (json-string->scm (response-body-port response))
        #f)))

;; Export functions
(define exports
  (list (cons 'export-program export-program)
        (cons 'import-program import-program)
        (cons 'export-explanation export-explanation)
        (cons 'import-explanation import-explanation)
        (cons 'call-external call-external)))
