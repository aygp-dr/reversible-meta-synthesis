(ns reversible-meta-synthesis.specs
  "Data specs for reversible-meta-synthesis (https://clojure.org/guides/spec).

  Terms follow the Clojure interpreter's conventions: variables are keywords
  named *x, constants are keywords named /a (build them with (keyword \"/a\");
  the reader rejects :/a), atoms are symbols, and compound terms are vectors.
  Function specs (s/fdef) live next to each defn."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]
            [reversible-meta-synthesis.clause :as-alias clause]
            [reversible-meta-synthesis.executable :as-alias executable]
            [reversible-meta-synthesis.explanation :as-alias explanation]))

;; --- Generators ---
;; Built by fns, not held in vars: building one loads test.check, which is
;; only on the :dev/:test classpath.

(defn- gen-variable []
  (gen/fmap #(keyword (str "*" %)) (gen/elements ["x" "y" "l" "l1" "l2" "ans"])))

(defn- gen-constant []
  (gen/fmap #(keyword (str "/" %)) (gen/elements ["a" "b" "c" "d"])))

(defn- gen-value []
  (gen/one-of [(gen-variable) (gen-constant) (gen/elements [:a :b [] 1 2 "s"])]))

(defn- gen-goal [preds]
  (gen/fmap (fn [[p args]] (into [p] args))
            (gen/tuple (gen/elements preds) (gen/vector (gen-value) 0 3))))

;; --- Terms ---

(s/def ::variable (s/with-gen (s/and keyword? #(str/starts-with? (name %) "*")) gen-variable))
(s/def ::constant (s/with-gen (s/and keyword? #(str/starts-with? (name %) "/")) gen-constant))

;; A variable, constant, atom or value, or a compound term (a vector).
(s/def ::term
  (s/with-gen
    (s/nonconforming
     (s/or :keyword keyword? :atom symbol? :number number? :string string? :nil nil?
           :compound (s/coll-of ::term :kind sequential?)))
    #(gen/one-of [(gen-value) (gen-goal ['append 'app3 'p 'q])])))

;; A goal: a predicate symbol applied to argument terms.
(s/def ::goal (s/with-gen (s/and vector? seq) #(gen-goal ['p 'q 'r])))

(s/def ::env (s/map-of ::variable ::term :gen-max 3))
(s/def ::body (s/coll-of ::goal :kind sequential? :gen-max 2))

;; --- Clauses and programs ---

(s/def ::clause/head ::term)
(s/def ::clause/body ::body)

;; Generated bodies call predicates (r, s) that have no clauses, so
;; evaluation terminates; recursive programs may not, as in Prolog.
(defn- gen-clause-parts []
  (gen/tuple (gen-goal ['p 'q]) (gen/vector (gen-goal ['r 's]) 0 2)))

(s/def ::clause
  (s/with-gen (s/keys :req-un [::clause/head ::clause/body])
    #(gen/fmap (fn [[h b]]
                 ((requiring-resolve 'reversible-meta-synthesis.reversible-interpreter/create-clause) h b))
               (gen-clause-parts))))

(s/def ::clauses (s/coll-of ::clause :kind sequential? :gen-max 4))

;; A clause as written in a program: [head body].
(s/def ::clause-pair (s/with-gen (s/tuple ::term ::body) gen-clause-parts))
(s/def ::program (s/coll-of ::clause-pair :kind sequential? :gen-max 4))

;; find-matching-clauses: the goal is sometimes one of the clause heads.
(s/def ::match-args
  (s/with-gen (s/cat :clauses ::clauses :goal ::term)
    #(gen/bind (s/gen ::clauses)
               (fn [cs]
                 (gen/tuple (gen/return cs)
                            (if (seq cs)
                              (gen/one-of [(gen/elements (mapv :head cs)) (gen-goal ['p 'q])])
                              (gen-goal ['p 'q])))))))

;; --- Explanations ---

(s/def ::explanation/goal ::term)
(s/def ::explanation/children (s/coll-of ::explanation :kind vector?))

;; s/keys does not bound recursion, so explanation trees get a depth-limited
;; generator.
(defn- gen-explanation [depth]
  (gen/fmap (fn [[goal children]] {:goal goal :children children})
            (gen/tuple (gen-goal ['p 'q 'r])
                       (if (zero? depth)
                         (gen/return [])
                         (gen/vector (gen-explanation (dec depth)) 0 2)))))

(s/def ::explanation
  (s/with-gen (s/keys :req-un [::explanation/goal ::explanation/children])
    #(gen-explanation 3)))

(s/def ::executable/id symbol?)
(s/def ::executable/goal ::term)
(s/def ::executable/children (s/coll-of ::executable :kind vector?))
(s/def ::executable (s/keys :req-un [::executable/id ::executable/goal ::executable/children]))

;; predicate -> composability value
(s/def ::composability (s/map-of any? nat-int? :gen-max 4))

;; --- Helpers for :fn relations ---

(defn explanation-nodes
  "Every node of an explanation tree."
  [e]
  (tree-seq #(seq (:children %)) :children e))

(defn term-leaves
  "The non-compound parts of a term."
  [t]
  (remove sequential? (tree-seq sequential? seq t)))

(defn extends-env?
  "s/fdef :fn: the result is nil (no match) or keeps every binding in :env."
  [{{:keys [env]} :args ret :ret}]
  (or (nil? ret) (every? (fn [[k v]] (= v (get ret k))) env)))
