(ns reversible-meta-synthesis.reversible-interpreter
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [reversible-meta-synthesis.specs :as specs]))

;; Environment management
(defn make-env []
  {})

(s/fdef make-env
  :args (s/cat)
  :ret (s/and ::specs/env empty?))

(defn lookup-var [env var]
  (get env var))

(s/fdef lookup-var
  :args (s/cat :env ::specs/env :var ::specs/variable)
  :ret (s/nilable ::specs/term))

(defn extend-env [env var val]
  (assoc env var val))

(s/fdef extend-env
  :args (s/cat :env ::specs/env :var ::specs/variable :val ::specs/term)
  :ret ::specs/env
  :fn (fn [{{:keys [var val]} :args ret :ret}]
        (= val (lookup-var ret var))))

;; Predicates for syntax
(defn variable? [x]
  (and (keyword? x) (str/starts-with? (name x) "*")))

(s/fdef variable?
  :args (s/cat :x any?)
  :ret boolean?)

(defn constant? [x]
  (and (keyword? x) (str/starts-with? (name x) "/")))

(s/fdef constant?
  :args (s/cat :x any?)
  :ret boolean?
  ;; nothing is both a variable and a constant
  :fn (fn [{{:keys [x]} :args ret :ret}]
        (not (and ret (variable? x)))))

(defn real-value [x]
  (if (constant? x)
    (keyword (subs (name x) 1))
    x))

(s/fdef real-value
  :args (s/cat :x ::specs/term)
  :ret ::specs/term
  ;; a constant /a stands for the value :a; anything else is itself
  :fn (fn [{{:keys [x]} :args ret :ret}]
        (if (constant? x)
          (= (name ret) (subs (name x) 1))
          (= ret x))))

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

(s/fdef match-head
  :args (s/cat :pattern ::specs/term :term ::specs/term :env (s/? ::specs/env))
  :ret (s/nilable ::specs/env)
  :fn specs/extends-env?)

(defn find-matching-clauses [clauses goal]
  (filter #(match-head (:head %) goal) clauses))

(s/fdef find-matching-clauses
  :args ::specs/match-args
  :ret ::specs/clauses
  ;; exactly the clauses whose head matches the goal
  :fn (fn [{{:keys [clauses goal]} :args ret :ret}]
        (= (set ret) (set (filter #(match-head (:head %) goal) clauses)))))

(declare eval-body)

(defn eval-goal
  "Evaluate a goal against a set of clauses with the current environment"
  [goal env clauses]
  (let [matching (find-matching-clauses clauses goal)]
    (when-let [clause (first matching)]
      (let [new-env (match-head (:head clause) goal env)]
        (eval-body (:body clause) new-env clauses)))))

(s/fdef eval-goal
  :args (s/cat :goal ::specs/goal :env ::specs/env :clauses ::specs/clauses)
  :ret (s/nilable ::specs/env)
  :fn specs/extends-env?)

(defn eval-body
  "Evaluate the body of a clause with the current environment"
  [body env clauses]
  (if (empty? body)
    env
    (when-let [new-env (eval-goal (first body) env clauses)]
      (eval-body (rest body) new-env clauses))))

(s/fdef eval-body
  :args (s/cat :body ::specs/body :env ::specs/env :clauses ::specs/clauses)
  :ret (s/nilable ::specs/env)
  :fn (s/and specs/extends-env?
             ;; an empty body succeeds with the env unchanged
             (fn [{{:keys [body env]} :args ret :ret}]
               (or (seq body) (= env ret)))))

;; Main interpreter functions
(defn create-clause
  "Create a clause from head and body"
  [head body]
  (->Clause head body))

(s/fdef create-clause
  :args (s/cat :head ::specs/term :body ::specs/body)
  :ret ::specs/clause
  :fn (fn [{{:keys [head body]} :args ret :ret}]
        (and (= head (:head ret)) (= body (:body ret)))))

(defn prolog
  "Main function for the reversible interpreter.
   In execution mode: given clauses and queries, returns results.
   In synthesis mode: given queries and expected results, returns clauses."
  [clauses queries]
  (let [clause-records (map (fn [[h b]] (create-clause h b)) clauses)]
    (map #(eval-goal % (make-env) clause-records) queries)))

(s/fdef prolog
  :args (s/cat :clauses ::specs/program
               :queries (s/coll-of ::specs/goal :kind sequential? :gen-max 3))
  :ret (s/coll-of (s/nilable ::specs/env))
  ;; one answer (or nil) per query
  :fn (fn [{{:keys [queries]} :args ret :ret}]
        (= (count queries) (count ret))))

;; Synthesis mode
(defn synthesize
  "Program synthesis from examples"
  [examples]
  (let [query-value-pairs (map (fn [[q v]] [q v]) examples)]
    ;; Implementation would search for clauses that satisfy all examples
    (println "Program synthesis from" (count examples) "examples")))

(s/fdef synthesize
  :args (s/cat :examples sequential?)
  :ret nil?)
