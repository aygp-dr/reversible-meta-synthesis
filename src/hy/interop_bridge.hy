#!/usr/bin/env hy

(import [json])
(import [requests])
(import [src.hy.reversible_interpreter [Interpreter variable? constant?]])
(import [src.hy.ebg [ExplanationNode]])

(defn export-program [clauses]
  "Convert a program (list of clauses) to JSON"
  (defn clause-to-json [[head body]]
    {"head" (term-to-json head)
     "body" (list (map term-to-json body))})
  
  (list (map clause-to-json clauses)))

(defn import-program [json-data]
  "Convert JSON back to a program"
  (defn json-to-clause [clause-json]
    [(json-to-term (get clause-json "head"))
     (list (map json-to-term (get clause-json "body")))])
  
  (list (map json-to-clause json-data)))

(defn term-to-json [term]
  "Convert a term to JSON"
  (cond
    [(variable? term) {"type" "var" "name" (name term)}]
    [(constant? term) {"type" "const" "name" (name term)}]
    [(symbol? term) {"type" "atom" "value" (name term)}]
    [(string? term) {"type" "string" "value" term}]
    [(number? term) {"type" "number" "value" term}]
    [(list? term) {"type" "list" 
                   "value" (list (map term-to-json term))}]
    [True {"type" "unknown"}]))

(defn json-to-term [json-data]
  "Convert JSON back to a term"
  (let [type (get json-data "type")]
    (cond
      [(= type "var") (symbol (+ "*" (get json-data "name")))]
      [(= type "const") (symbol (+ "/" (get json-data "name")))]
      [(= type "atom") (symbol (get json-data "value"))]
      [(= type "string") (get json-data "value")]
      [(= type "number") (get json-data "value")]
      [(= type "list") (list (map json-to-term (get json-data "value")))]
      [True None])))

(defn export-explanation [explanation]
  "Convert an explanation tree to JSON"
  (defn node-to-json [node]
    {"goal" (term-to-json (. node goal))
     "children" (list (map node-to-json (. node children)))})
  
  (node-to-json explanation))

(defn import-explanation [json-data]
  "Convert JSON back to an explanation tree"
  (defn json-to-node [node-json]
    (ExplanationNode
      (json-to-term (get node-json "goal"))
      (list (map json-to-node (get node-json "children")))))
  
  (json-to-node json-data))

(defn call-external [language function input]
  "Call external implementation via HTTP"
  (let [url (+ f"http://localhost:8080/api/{language}/{function}")
        response (.post requests url :json input)]
    (if (= (. response status_code) 200)
        (.json response)
        None)))
