(ns reversible-meta-synthesis.interop-bridge-test
  (:require [clojure.test :refer :all]
            [reversible-meta-synthesis.interop-bridge :as bridge]
            [reversible-meta-synthesis.ebg :as ebg]))

(deftest export-import-program-test
  (testing "Export and import of programs via JSON"
    (let [program [['append [] '*l '*l] []]
                  ['append ['*x '& '*l1] '*l2 ['*x '& '*l3]] 
                   [['append '*l1 '*l2 '*l3]]]]
          
          ;; Export to JSON
          json-data (bridge/export-program program)
          
          ;; Import back
          imported-program (bridge/import-program json-data)]
      
      ;; Should match the original
      (is (= program imported-program)))))

(deftest export-import-explanation-test
  (testing "Export and import of explanation trees via JSON"
    (let [explanation (ebg/->ExplanationNode
                        ['append ['/a] ['/b] ['/a '/b]]
                        [(ebg/->ExplanationNode 
                           ['append [] ['/b] ['/b]] 
                           [])])
          
          ;; Export to JSON
          json-data (bridge/export-explanation explanation)
          
          ;; Import back
          imported-explanation (bridge/import-explanation json-data)]
      
      ;; Check structure
      (is (= (:goal explanation) (:goal imported-explanation)))
      (is (= (count (:children explanation)) 
             (count (:children imported-explanation)))))))

(defn -main []
  (run-tests 'reversible-meta-synthesis.interop-bridge-test))
