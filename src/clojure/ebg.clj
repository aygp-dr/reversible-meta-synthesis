(ns reversible-meta-synthesis.ebg
  (:require [reversible-meta-synthesis.reversible-interpreter :as ri]))

;; Explanation tree structure
(defrecord ExplanationNode [goal children])

(defn make-explanation-node [goal children]
  (->ExplanationNode goal children))

;; Building explanations
(defn build-explanation
  "Build an explanation tree for a given goal using available clauses"
  [goal clauses]
  (let [matching-clauses (ri/find-matching-clauses clauses goal)]
    (if (empty? matching-clauses)
      (make-explanation-node goal [])
      (let [clause (first matching-clauses)
            matched-env (ri/match-head (:head clause) goal)]
        (make-explanation-node 
          goal
          (mapv #(build-explanation % clauses) (:body clause)))))))

;; Generalization
(defn generalize-term
  "Generalize a term by replacing constants with variables"
  [term]
  (cond
    (ri/constant? term) (symbol (str "*GEN" (subs (name term) 1)))
    (symbol? term) term
    (sequential? term) (mapv generalize-term term)
    :else term))

(defn generalize-explanation
  "Generalize an explanation tree"
  [expl-tree]
  (make-explanation-node
    (generalize-term (:goal expl-tree))
    (mapv generalize-explanation (:children expl-tree))))

;; Decomposition based on composability
(defn decompose-explanation
  "Decompose an explanation based on composability values"
  [explanation composability decomp-force]
  (let [goal (:goal explanation)
        comp-value (get composability goal)
        should-decompose (and comp-value (<= comp-value decomp-force))]
    (if should-decompose
      (mapcat #(decompose-explanation % composability decomp-force) 
              (:children explanation))
      [explanation])))

;; Executable explanation
(defn create-executable-explanation
  "Create an executable version of the explanation tree"
  [explanation]
  (let [node-id (gensym "node")]
    {:id node-id
     :goal (:goal explanation)
     :children (mapv create-executable-explanation (:children explanation))}))

(defn apply-explanation
  "Apply an executable explanation to synthesize a program"
  [explanation target-goal]
  (println "Applying explanation to synthesize a program for" target-goal))
