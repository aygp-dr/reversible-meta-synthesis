(ns reversible-meta-synthesis.composability)

;; Composability values for different predicates
(def composability-values
  {'prolog 1
   'goalseq 3
   'goal 1
   'search 1
   'head 4
   'head1 4
   'bind 4
   'fetch 2
   'fetch1 3})

(defn get-composability
  "Get the composability value for a goal"
  [goal]
  (let [pred (if (sequential? goal) (first goal) goal)]
    (get composability-values pred 0)))

(defn should-decompose?
  "Determine if a goal should be decomposed based on its composability"
  [goal decomp-force]
  (<= (get-composability goal) decomp-force))

;; Analysis functions to automatically determine composability
(defn analyze-dependencies
  "Analyze dependencies between predicates to determine composability"
  [clauses]
  (let [dependencies (atom {})]
    ;; Build dependency graph
    (doseq [[head body] clauses]
      (let [head-pred (if (sequential? head) (first head) head)]
        (doseq [goal body]
          (let [goal-pred (if (sequential? goal) (first goal) goal)]
            (swap! dependencies update head-pred (fnil conj #{}) goal-pred)))))
    
    ;; Calculate composability based on dependency structure
    (let [dep-graph @dependencies
          predicates (keys dep-graph)
          composability (atom {})]
      (doseq [pred predicates]
        (let [depth (atom 0)
              visited (atom #{})]
          ;; Calculate depth of predicate in dependency graph
          (letfn [(calculate-depth [p]
                    (when-not (@visited p)
                      (swap! visited conj p)
                      (if-let [deps (get dep-graph p)]
                        (do
                          (doseq [dep deps]
                            (calculate-depth dep))
                          (swap! depth inc))
                        (swap! depth inc))))]
            (calculate-depth pred)
            (swap! composability assoc pred @depth))))
      
      @composability)))
