(ns reversible-meta-synthesis.ebg
  (:require [clojure.spec.alpha :as s]
            [reversible-meta-synthesis.reversible-interpreter :as ri]
            [reversible-meta-synthesis.specs :as specs]))

;; Explanation tree structure
(defrecord ExplanationNode [goal children])

(defn make-explanation-node [goal children]
  (->ExplanationNode goal children))

(s/fdef make-explanation-node
  :args (s/cat :goal ::specs/term :children (s/coll-of ::specs/explanation :kind vector?))
  :ret ::specs/explanation)

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

(s/fdef build-explanation
  :args (s/cat :goal ::specs/goal :clauses ::specs/clauses)
  :ret ::specs/explanation
  :fn (fn [{{:keys [goal]} :args ret :ret}]
        (= goal (:goal ret))))

;; Generalization
(defn generalize-term
  "Generalize a term by replacing constants with variables"
  [term]
  (cond
    (ri/constant? term) (symbol (str "*GEN" (subs (name term) 1)))
    (symbol? term) term
    (sequential? term) (mapv generalize-term term)
    :else term))

(s/fdef generalize-term
  :args (s/cat :term ::specs/term)
  :ret ::specs/term
  ;; no constants survive, and generalizing again changes nothing
  :fn (fn [{ret :ret}]
        (and (not-any? ri/constant? (specs/term-leaves ret))
             (= ret (generalize-term ret)))))

(defn generalize-explanation
  "Generalize an explanation tree"
  [expl-tree]
  (make-explanation-node
   (generalize-term (:goal expl-tree))
   (mapv generalize-explanation (:children expl-tree))))

(s/fdef generalize-explanation
  :args (s/cat :expl-tree ::specs/explanation)
  :ret ::specs/explanation
  :fn (fn [{{:keys [expl-tree]} :args ret :ret}]
        (= (count (specs/explanation-nodes expl-tree))
           (count (specs/explanation-nodes ret)))))

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

(s/fdef decompose-explanation
  :args (s/cat :explanation ::specs/explanation :composability ::specs/composability
               :decomp-force int?)
  :ret (s/coll-of ::specs/explanation)
  ;; never more pieces than nodes in the tree
  :fn (fn [{{:keys [explanation]} :args ret :ret}]
        (<= (count ret) (count (specs/explanation-nodes explanation)))))

;; Executable explanation
(defn create-executable-explanation
  "Create an executable version of the explanation tree"
  [explanation]
  (let [node-id (gensym "node")]
    {:id node-id
     :goal (:goal explanation)
     :children (mapv create-executable-explanation (:children explanation))}))

(s/fdef create-executable-explanation
  :args (s/cat :explanation ::specs/explanation)
  :ret ::specs/executable
  :fn (fn [{{:keys [explanation]} :args ret :ret}]
        (= (count (specs/explanation-nodes explanation))
           (count (specs/explanation-nodes ret)))))

(defn apply-explanation
  "Apply an executable explanation to synthesize a program"
  [explanation target-goal]
  (println "Applying explanation to synthesize a program for" target-goal))

(s/fdef apply-explanation
  :args (s/cat :explanation map? :target-goal ::specs/term)
  :ret nil?)
