#!/usr/bin/env hy

(import [unittest [TestCase main]])
(import [src.hy.interop_bridge [
    export-program 
    import-program
    export-explanation
    import-explanation
    term-to-json
    json-to-term
]])
(import [src.hy.ebg [ExplanationNode]])

(defclass InteropBridgeTests [TestCase]
  (defn test-export-import-program [self]
    "Test that programs can be exported to JSON and imported back"
    (setv program [
      [["append" [] "*l" "*l"] []]
      [["append" ["*x" "*l1"] "*l2" ["*x" "*l3"]] 
       [["append" "*l1" "*l2" "*l3"]]]
    ])
    
    ; Export to JSON
    (setv json-data (export-program program))
    
    ; Import back
    (setv imported-program (import-program json-data))
    
    ; Should match the original
    (.assertEqual self (str program) (str imported-program)))
  
  (defn test-export-import-explanation [self]
    "Test that explanation trees can be exported to JSON and imported back"
    (setv explanation (ExplanationNode
      ["append" ["/a"] ["/b"] ["/a" "/b"]]
      [(ExplanationNode ["append" [] ["/b"] ["/b"]] [])]))
    
    ; Export to JSON
    (setv json-data (export-explanation explanation))
    
    ; Import back
    (setv imported-explanation (import-explanation json-data))
    
    ; Check structure
    (.assertEqual self 
                  (. explanation goal)
                  (. imported-explanation goal))
    (.assertEqual self 
                  (len (. explanation children))
                  (len (. imported-explanation children)))))

(defmain [&rest args]
  (main))
