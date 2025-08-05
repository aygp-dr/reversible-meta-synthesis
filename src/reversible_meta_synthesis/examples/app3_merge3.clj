(ns reversible-meta-synthesis.examples.app3-merge3
  (:require [reversible-meta-synthesis.reversible-interpreter :as ri]
            [reversible-meta-synthesis.ebg :as ebg]
            [reversible-meta-synthesis.composability :as comp]))

;; Define the append and app3 programs
(def append-clauses
  [[['append [] '*x '*x] []]
   [['append ['*x '& '*l1] '*l2 ['*x '& '*l3]] 
    [['append '*l1 '*l2 '*l3]]]])

(def app3-clauses
  [[['app3 '*x '*y '*z '*a] 
    [['append '*x '*y '*aa] ['append '*aa '*z '*a]]]])

;; Define the merge program
(def merge-clauses
  [[['merge [] '*x '*x] []]
   [['merge '*x [] '*x] []]
   [['merge ['*x '& '*l1] ['*y '& '*l2] ['*x '& '*l3]] 
    [['<= '*x '*y] ['merge '*l1 ['*y '& '*l2] '*l3]]]
   [['merge ['*x '& '*l1] ['*y '& '*l2] ['*y '& '*l3]] 
    [[> '*x '*y] ['merge ['*x '& '*l1] '*l2 '*l3]]]])

(defn example-app3 []
  (println "Running app3 program in execution mode:")
  
  (let [all-clauses (concat append-clauses app3-clauses)
        queries [[['app3 ['/a] ['/b] ['/c] '*ans]]]
        results (ri/prolog all-clauses queries)]
    
    (println "Query:" (first queries))
    (println "Result:" (first results))))

(defn synthesize-merge3 []
  (println "\nSynthesizing merge3 program from app3 explanation:")
  
  ;; Build explanation for app3
  (let [all-clauses (concat append-clauses app3-clauses)
        goal ['app3 ['/a] ['/b] ['/c] ['/a '/b '/c]]
        explanation (ebg/build-explanation goal all-clauses)
        
        ;; Generalize and decompose explanation
        gen-expl (ebg/generalize-explanation explanation)
        decomposed (ebg/decompose-explanation 
                     gen-expl 
                     comp/composability-values 
                     1) ;; DECOMP_FORCE = 1 for clause-level
        
        ;; Create executable explanation
        exec-expl (ebg/create-executable-explanation (first decomposed))
        
        ;; Synthesize merge3 using the explanation
        target-goal ['merge3 ['/3] ['/1] ['/2] ['/1 '/2 '/3]]
        synthesized (ebg/apply-explanation exec-expl target-goal)]
    
    (println "Target:" target-goal)
    (println "Synthesized program structure:" 
             '[['merge3 *x *y *z *a] [['merge *x *y *aa] ['merge *aa *z *a]]])))

(defn -main [& args]
  (example-app3)
  (synthesize-merge3))
