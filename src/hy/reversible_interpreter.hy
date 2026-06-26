#!/usr/bin/env hy
(import collections.abc [Mapping Sequence])

(defclass Env []
  (defn __init__ [self [bindings {}]]
    (setv self.bindings bindings))

  (defn lookup [self var]
    (.get self.bindings var None))

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
    (lfor clause self.clauses :if (self.match-head (get clause 0) (get goal 0)) clause))

  (defn match-head [self pattern term [env (Env)]]
    (cond
      (variable? pattern)
      (let [val (.lookup env pattern)]
        (if (is val None)
            (.extend env pattern term)
            (and (= val term) env)))
      (constant? pattern)
      (if (= (real-value pattern) term) env None)
      (and (isinstance pattern Sequence) (isinstance term Sequence))
      (if (!= (len pattern) (len term))
          None
          (self.match-sequence (cut pattern 1) (cut term 1)
                               (self.match-head (get pattern 0) (get term 0) env)))
      (= pattern term) env
      True None))

  (defn match-sequence [self patterns terms env]
    (if (or (is env None) (not patterns))
        env
        (self.match-sequence
          (cut patterns 1)
          (cut terms 1)
          (self.match-head (get patterns 0) (get terms 0) env))))

  (defn eval-goal [self goal env]
    (setv matching-clauses (self.find-matching-clauses goal))
    (if matching-clauses
        (let [clause (get matching-clauses 0)
              head (get clause 0)
              body (get clause 1)
              new-env (self.match-head head goal env)]
          (self.eval-body body new-env))
        None))

  (defn eval-body [self body env]
    (if (not body)
        env
        (let [new-env (self.eval-goal (get body 0) env)]
          (if new-env
              (self.eval-body (cut body 1) new-env)
              None))))

  (defn query [self goal]
    (self.eval-goal goal (Env)))

  (defn synthesize [self example-inputs example-outputs]
    "Synthesize a program from input-output examples"))

(defn reverse-interpreter [clauses queries]
  "Implementation of the reversible interpreter.
   In execution mode: given clauses and queries, returns results.
   In synthesis mode: given queries and expected results, returns clauses."
  (let [interp (Interpreter)]
    (for [clause clauses]
      (.add-clause interp (get clause 0) (get clause 1)))

    (lfor query queries
          (.eval-goal interp query (Env)))))
