(ns reversible-meta-synthesis.reversible-interpreter-test
  (:require [clojure.test :refer :all]
            [reversible-meta-synthesis.reversible-interpreter :as ri]))

(deftest variable-predicates-test
  (testing "Testing variable predicates"
    (is (ri/variable? '*x) "Symbol starting with * should be a variable")
    (is (not (ri/variable? 'x)) "Regular symbol should not be a variable")
    (is (ri/constant? '/x) "Symbol starting with / should be a constant")
    (is (not (ri/constant? 'x)) "Regular symbol should not be a constant")))

(deftest match-head-test
  (testing "Match pattern against term"
    (is (ri/match-head '*x 'value) "Variable should match any value")
    (is (= {'*x 'value} (ri/match-head '*x 'value)) "Environment should contain binding")
    (is (ri/match-head '/x 'x) "Constant should match its value")
    (is (not (ri/match-head '/x 'y)) "Constant should not match different value")
    (is (ri/match-head ['*x '*y] ['a 'b]) "List with variables should match")
    (is (not (ri/match-head ['*x '*y] ['a])) "Lists of different lengths should not match")))

(deftest eval-goal-test
  (testing "Evaluating goals against clauses"
    (let [clauses [(ri/create-clause ['append [] '*l '*l] [])
                   (ri/create-clause ['append ['*x '& '*l1] '*l2 ['*x '& '*l3]] 
                                     [['append '*l1 '*l2 '*l3]])]]
      
      (is (ri/eval-goal ['append [] ['/a '/b] '*ans] {} clauses) 
          "Base case should match")
      
      (is (= {'*l ['/a '/b] '*ans ['/a '/b]} 
             (ri/eval-goal ['append [] ['/a '/b] '*ans] {} clauses))
          "Environment should contain correct bindings")
      
      (is (ri/eval-goal ['append ['/a] ['/b] '*ans] {} clauses)
          "Recursive case should match")
      
      (is (= {'*ans ['/a '/b]}
             (ri/eval-goal ['append ['/a] ['/b] '*ans] {} clauses))
          "Recursive evaluation should return correct result"))))

(deftest prolog-test
  (testing "Full prolog execution"
    (let [clauses [[['append [] '*l '*l] []]
                   [['append ['*x '& '*l1] '*l2 ['*x '& '*l3]] 
                    [['append '*l1 '*l2 '*l3]]]]
          
          queries [[['append ['/a '/b] ['/c '/d] '*ans]]]
          expected-results [{'*ans ['/a '/b '/c '/d]}]]
      
      (is (= expected-results (ri/prolog clauses queries))
          "Prolog should return correct results for queries"))))

(defn -main []
  (run-tests 'reversible-meta-synthesis.reversible-interpreter-test))
