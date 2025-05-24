#!/usr/bin/env hy
(import [collections.abc [Mapping Sequence]])

(defclass Env []
  (defn __init__ [self &optional [bindings {}]]
    (setv self.bindings bindings))
    
  (defn lookup [self var]
    (get self.bindings var None))
    
  (defn extend [self var val]
    (Env (| self.bindings {var val}))))

(defn atom? [x]
  (not (isinstance x Sequence)))

(defn variable? [x]
  (and (isinstance x str) (.startswith x "*")))

(defn constant? [x]
  (and (isinstance x str) (.startswith x "/")))

(defn real-value [x]
  (if (constant? x)
      (cut x 1)
      x))

(defclass Interpreter []
  (defn __init__ [self]
    (setv self.clauses []))
    
  (defn add-clause [self head body]
    (.append self.clauses [head body]))
    
  (defn find-matching-clauses [self goal]
    (lfor clause self.clauses :if (self.match-head (first clause) (first goal)) clause))
    
  (defn match-head [self pattern term &optional [env (Env)]]
    (cond 
      [(variable? pattern) 
       (let [val (.lookup env pattern)]
         (if (is val None)
             (.extend env pattern term)
             (and (= val term) env)))]
      [(constant? pattern) 
       (if (= (real-value pattern) term) env None)]
      [(and (isinstance pattern Sequence) (isinstance term Sequence))
       (if (!= (len pattern) (len term))
           None
           (self.match-sequence (rest pattern) (rest term)
                              (self.match-head (first pattern) (first term) env)))]
      [(= pattern term) env]
      [True None]))
      
  (defn match-sequence [self patterns terms env]
    (if (or (is env None) (not patterns))
        env
        (self.match-sequence 
          (rest patterns) 
          (rest terms) 
          (self.match-head (first patterns) (first terms) env))))
    
  (defn eval-goal [self goal env]
    (setv matching-clauses (self.find-matching-clauses self goal))
    (if matching-clauses
        (let [[head body] (first matching-clauses)
              new-env (self.match-head head goal env)]
          (self.eval-body body new-env))
        None))
    
  (defn eval-body [self body env]
    (if (not body)
        env
        (let [new-env (self.eval-goal self (first body) env)]
          (if new-env
              (self.eval-body (rest body) new-env)
              None))))
              
  (defn query [self goal]
    (self.eval-goal self goal (Env)))
    
  (defn synthesize [self example-inputs example-outputs]
    "Synthesize a program from input-output examples"))

(defn reverse-interpreter [clauses queries]
  "Implementation of the reversible interpreter.
   In execution mode: given clauses and queries, returns results.
   In synthesis mode: given queries and expected results, returns clauses."
  (let [interp (Interpreter)]
    (for [[head body] clauses]
      (.add-clause interp head body))
    
    (lfor query queries
          (.eval-goal interp query (Env)))))
