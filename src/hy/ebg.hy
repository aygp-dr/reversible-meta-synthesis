#!/usr/bin/env hy

(import [src.hy.reversible_interpreter [Interpreter variable? constant?]])

(defclass ExplanationNode []
  (defn __init__ [self goal children]
    (setv self.goal goal
          self.children children))
  
  (defn print-tree [self &optional [indent 0]]
    (print (.join "" (list (* indent "  "))) self.goal)
    (for [child self.children]
      (.print-tree child (+ indent 1)))))

(defclass ExplanationBuilder []
  (defn __init__ [self interpreter]
    (setv self.interpreter interpreter))
  
  (defn build-explanation [self goal]
    "Build an explanation tree for a given goal"
    (let [matching-clauses (.find-matching-clauses self.interpreter goal)]
      (if matching-clauses
          (let [[head body] (first matching-clauses)
                matched-env (.match-head self.interpreter head goal)]
            (ExplanationNode goal
                            (lfor subgoal body
                                  (.build-explanation self subgoal))))
          (ExplanationNode goal []))))
  
  (defn generalize-explanation [self expl-tree]
    "Generalize an explanation tree by replacing constants with variables"
    (defn generalize-term [term]
      (cond
        [(constant? term) (+ "*GEN" (cut term 1))]
        [(isinstance term str) term]
        [(isinstance term list)
         (list (map generalize-term term))]
        [True term]))
    
    (ExplanationNode 
      (generalize-term (.goal expl-tree))
      (lfor child (.children expl-tree)
            (.generalize-explanation self child)))))

(defn decompose-explanation [explanation composability decomp-force]
  "Decompose an explanation based on composability values"
  (defn should-decompose [goal]
    (and (in goal composability)
         (<= (get composability goal) decomp-force)))
  
  (defn decompose-node [node]
    (if (should-decompose (. node goal))
        (lfor child (. node children)
              (decompose-node child))
        [node]))
  
  (decompose-node explanation))
