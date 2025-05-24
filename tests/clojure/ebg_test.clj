(ns reversible-meta-synthesis.ebg-test
  (:require [clojure.test :refer :all]
            [reversible-meta-synthesis.reversible-interpreter :as ri]
            [reversible-meta-synthesis.ebg :as ebg]))

(deftest explanation-tree-test
  (testing "Building explanation trees"
    (let [clauses [(ri/create-clause ['append [] '*l '*l] [])
                   (ri/create-clause ['append ['*x '& '*l1] '*l2 ['*x '& '*l3]] 
                                     [['append '*l1 '*l2 '*l3]])]
          
          goal ['append ['/a] ['/b] ['/a '/b]]
          explanation (ebg/build-explanation goal clauses)]
      
      (is explanation "Should build an explanation")
      (is (= goal (:goal explanation)) "Root goal should match input goal")
      (is (seq (:children explanation)) "Should have child explanations"))))

(deftest generalization-test
  (testing "Generalizing explanations"
    (let [clauses [(ri/create-clause ['append [] '*l '*l] [])
                   (ri/create-clause ['append ['*x '& '*l1] '*l2 ['*x '& '*l3]] 
                                     [['append '*l1 '*l2 '*l3]])]
          
          goal ['append ['/a] ['/b] ['/a '/b]]
          explanation (ebg/build-explanation goal clauses)
          generalized (ebg/generalize-explanation explanation)]
      
      (is generalized "Should generalize the explanation")
      (is (not= goal (:goal generalized)) "Generalized goal should differ from original")
      (is (= ['append ['*GENa] ['*GENb] ['*GENa '*GENb]] (:goal generalized))
          "Constants should be replaced with variables"))))

(deftest decomposition-test
  (testing "Decomposing explanations"
    (let [clauses [(ri/create-clause ['append [] '*l '*l] [])
                   (ri/create-clause ['append ['*x '& '*l1] '*l2 ['*x '& '*l3]] 
                                     [['append '*l1 '*l2 '*l3]])]
          
          goal ['append ['/a '/b] ['/c '/d] ['/a '/b '/c '/d]]
          explanation (ebg/build-explanation goal clauses)
          generalized (ebg/generalize-explanation explanation)
          decomposed (ebg/decompose-explanation 
                       generalized 
                       {'append 1} 
                       1)]
      
      (is (seq decomposed) "Should produce decomposed explanations")
      (is (<= (count decomposed) (count (tree-seq 
                                          #(seq (:children %)) 
                                          :children 
                                          generalized)))
          "Decomposition should not create more nodes than in the original tree"))))

(defn -main []
  (run-tests 'reversible-meta-synthesis.ebg-test))
