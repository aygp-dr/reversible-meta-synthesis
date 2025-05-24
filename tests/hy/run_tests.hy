#!/usr/bin/env hy

(import [src.hy.reversible_interpreter [reverse-interpreter Interpreter]])
(import [src.hy.ebg [ExplanationBuilder]])
(import [unittest [TestCase main]])

(defclass ReversibleInterpreterTests [TestCase]
  (defn test-basic-append-execution [self]
    "Test that the interpreter can execute a basic append program"
    (setv clauses 
      [[["append" [] "*l" "*l"] []]
       [["append" ["*x" "*l1"] "*l2" ["*x" "*l3"]] 
        [["append" "*l1" "*l2" "*l3"]]]])
       
    (setv queries [[["append" ["/a" "/b"] ["/c" "/d"] "*ans"]]])
    
    (setv results (reverse-interpreter clauses queries))
    (.assertEqual self (first results) {"*ans" ["/a" "/b" "/c" "/d"]}))
  
  (defn test-variable-binding [self]
    "Test that variables are properly bound in the environment"
    (setv interp (Interpreter))
    (setv env (.match-head interp "*var" "value" None))
    (.assertEqual self (.lookup env "*var") "value")))

(defclass EBGTests [TestCase]
  (defn test-explanation-building [self]
    "Test that we can build and generalize explanations"
    (setv interp (Interpreter))
    (.add-clause interp ["append" [] "*l" "*l"] [])
    (.add-clause interp 
      ["append" ["*x" "*l1"] "*l2" ["*x" "*l3"]] 
      [["append" "*l1" "*l2" "*l3"]])
    
    (setv builder (ExplanationBuilder interp))
    (setv expl (.build-explanation builder ["append" ["/a"] ["/b"] "*ans"]))
    
    (.assertIsNotNone self expl)
    ; Test generalization
    (setv gen-expl (.generalize-explanation builder expl))
    (.assertNotEqual self 
                     (.goal expl) 
                     (.goal gen-expl)
                     "Generalization should replace constants with variables")))

(defmain [&rest args]
  (main))
