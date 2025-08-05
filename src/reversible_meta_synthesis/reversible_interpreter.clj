(ns reversible-meta-synthesis.reversible-interpreter
  (:require [clojure.string :as str]))

;; Environment management
(defn make-env []
  {})

(defn lookup-var [env var]
  (get env var))

(defn extend-env [env var val]
  (assoc env var val))

;; Predicates for syntax
(defn variable? [x]
  (and (keyword? x) (str/starts-with? (name x) "*")))

(defn constant? [x]
  (and (keyword? x) (str/starts-with? (name x) "/")))

(defn real-value [x]
  (if (constant? x)
    (keyword (subs (name x) 1))
    x))

;; Core reversible interpreter
(defrecord Clause [head body])

(defn match-head
  "Match a pattern (clause head) against a term (goal), returning updated env if successful"
  ([pattern term] (match-head pattern term (make-env)))
  ([pattern term env]
   (cond
     (variable? pattern)
     (let [val (lookup-var env pattern)]
       (if val
         (if (= val term) env nil)
         (extend-env env pattern term)))
     
     (constant? pattern)
     (if (= (real-value pattern) term) env nil)
     
     (and (sequential? pattern) (sequential? term) (= (count pattern) (count term)))
     (reduce 
       (fn [env' [p t]] 
         (if env'
           (match-head p t env')
           (reduced nil)))
       env
       (map vector pattern term))
     
     (= pattern term) env
     
     :else nil)))

(defn find-matching-clauses [clauses goal]
  (filter #(match-head (:head %) (first goal)) clauses))

(declare eval-body)

(defn eval-goal 
  "Evaluate a goal against a set of clauses with the current environment"
  [goal env clauses]
  (let [matching (find-matching-clauses clauses goal)]
    (when-let [clause (first matching)]
      (let [new-env (match-head (:head clause) goal env)]
        (eval-body (:body clause) new-env clauses)))))

(defn eval-body
  "Evaluate the body of a clause with the current environment"
  [body env clauses]
  (if (empty? body)
    env
    (when-let [new-env (eval-goal (first body) env clauses)]
      (eval-body (rest body) new-env clauses))))

;; Main interpreter functions
(defn create-clause 
  "Create a clause from head and body"
  [head body]
  (->Clause head body))

(defn prolog 
  "Main function for the reversible interpreter.
   In execution mode: given clauses and queries, returns results.
   In synthesis mode: given queries and expected results, returns clauses."
  [clauses queries]
  (let [clause-records (map (fn [[h b]] (create-clause h b)) clauses)]
    (map #(eval-goal % (make-env) clause-records) queries)))

;; Synthesis mode
(defn synthesize 
  "Program synthesis from examples"
  [examples]
  (let [query-value-pairs (map (fn [[q v]] [q v]) examples)]
    ;; Implementation would search for clauses that satisfy all examples
    (println "Program synthesis from" (count examples) "examples")))
