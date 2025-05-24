#!/usr/bin/env python3

from flask import Flask, request, jsonify
import subprocess
import json
import os
import tempfile

app = Flask(__name__)

# Configuration for each language implementation
IMPLEMENTATIONS = {
    "prolog": {
        "execute": ["swipl", "-q", "-t", "main", "-f"],
        "file_ext": ".pl"
    },
    "hy": {
        "execute": ["hy"],
        "file_ext": ".hy"
    },
    "scheme": {
        "execute": ["guile"],
        "file_ext": ".scm"
    },
    "clojure": {
        "execute": ["clojure", "-m"],
        "file_ext": ".clj"
    }
}

@app.route('/api/<language>/<function>', methods=['POST'])
def call_implementation(language, function):
    """
    Call a function in a specific language implementation
    """
    if language not in IMPLEMENTATIONS:
        return jsonify({"error": f"Unsupported language: {language}"}), 400
    
    # Get implementation config
    impl = IMPLEMENTATIONS[language]
    
    # Create temporary file with input data
    input_data = request.json
    
    with tempfile.NamedTemporaryFile(suffix=impl["file_ext"], mode='w', delete=False) as f:
        # Write appropriate code for each language
        if language == "prolog":
            f.write(f"""
:- consult('../../src/prolog/interop_bridge.pl').
:- consult('../../src/prolog/reversible_interpreter.pl').
:- consult('../../src/prolog/ebg.pl').

main :-
    % Parse input JSON
    atom_json_term('{input_json}', Input, []),
    % Call the requested function
    call_function('{function}', Input, Output),
    % Output the result as JSON
    atom_json_term(JSON, Output, []),
    write(JSON),
    halt.

call_function('execute', Input, Output) :-
    % Extract clauses and queries from input
    member(clauses=Clauses, Input),
    member(queries=Queries, Input),
    % Call the interpreter
    prolog(Clauses, Queries, Output).

call_function('synthesize', Input, Output) :-
    % Extract queries and expected values from input
    member(queries=Queries, Input),
    member(values=Values, Input),
    % Call the interpreter in synthesis mode
    prolog(Output, Queries, Values).

call_function('build_explanation', Input, Output) :-
    % Extract goal and clauses from input
    member(goal=Goal, Input),
    member(clauses=Clauses, Input),
    % Build the explanation
    build_explanation(Goal, Clauses, Output).

:- main.
            """.replace("{function}", function).replace("{input_json}", json.dumps(input_data)))
        
        elif language == "hy":
            f.write(f"""
#!/usr/bin/env hy
(import [json])
(import [src.hy.interop_bridge [import-program export-program]])
(import [src.hy.reversible_interpreter [reverse-interpreter]])
(import [src.hy.ebg [ExplanationBuilder]])

(defn call-function [function input-data]
  (cond
    [(= function "execute")
     (let [clauses (import-program (get input-data "clauses"))
           queries (import-program (get input-data "queries"))]
       (reverse-interpreter clauses queries))]
    
    [(= function "synthesize")
     (let [queries (import-program (get input-data "queries"))
           values (get input-data "values")]
       ; Synthesis implementation here
       {"message" "Program synthesis not fully implemented in Hy"})]
    
    [(= function "build_explanation")
     (let [goal (json-to-term (get input-data "goal"))
           clauses (import-program (get input-data "clauses"))
           interpreter (Interpreter)
           _ (for [[head body] clauses]
               (.add-clause interpreter head body))
           builder (ExplanationBuilder interpreter)
           explanation (.build-explanation builder goal)]
       (export-explanation explanation))]
    
    [True {{"error" (+ "Unknown function: " function)}}]))

(print (json.dumps (call-function "{function}" {input_json})))
            """.replace("{function}", function).replace("{input_json}", json.dumps(input_data)))
        
        elif language == "scheme":
            f.write(f"""
#!/usr/bin/env guile
!#

(use-modules (json))
(load "../../src/scheme/interop-bridge.scm")
(load "../../src/scheme/reversible-interpreter.scm")
(load "../../src/scheme/ebg.scm")

(define (call-function function input-data)
  (cond
    ((string=? function "execute")
     (let ((clauses (import-program (assoc-ref input-data "clauses")))
           (queries (import-program (assoc-ref input-data "queries"))))
       (prolog clauses queries)))
    
    ((string=? function "synthesize")
     (let ((queries (import-program (assoc-ref input-data "queries")))
           (values (assoc-ref input-data "values")))
       ;; Synthesis implementation here
       '(("message" . "Program synthesis not fully implemented in Scheme"))))
    
    ((string=? function "build_explanation")
     (let ((goal (json->term (assoc-ref input-data "goal")))
           (clauses (import-program (assoc-ref input-data "clauses"))))
       (export-explanation (build-explanation goal clauses))))
    
    (else
     `(("error" . ,(string-append "Unknown function: " function))))))

(display (scm->json-string (call-function "{function}" '{input_json})))
            """.replace("{function}", function).replace("{input_json}", json.dumps(input_data)))
        
        elif language == "clojure":
            f.write(f"""
(ns interop-bridge-runner
  (:require [clojure.data.json :as json]
            [reversible-meta-synthesis.interop-bridge :as bridge]
            [reversible-meta-synthesis.reversible-interpreter :as ri]
            [reversible-meta-synthesis.ebg :as ebg]))

(defn call-function [function input-data]
  (case function
    "execute"
    (let [clauses (bridge/import-program (:clauses input-data))
          queries (bridge/import-program (:queries input-data))]
      (ri/prolog clauses queries))
    
    "synthesize"
    (let [queries (bridge/import-program (:queries input-data))
          values (:values input-data)]
      ;; Synthesis implementation here
      {:message "Program synthesis not fully implemented in Clojure"})
    
    "build_explanation"
    (let [goal (bridge/json->term (:goal input-data))
          clauses (bridge/import-program (:clauses input-data))
          explanation (ebg/build-explanation goal clauses)]
      (bridge/export-explanation explanation))
    
    {:error (str "Unknown function: " function)}))

(println (json/write-str (call-function "{function}" {input_json})))
            """.replace("{function}", function).replace("{input_json}", json.dumps(input_data)))
        
        f_path = f.name
    
    try:
        # Execute the script
        result = subprocess.run(
            impl["execute"] + [f_path], 
            capture_output=True,
            text=True
        )
        
        # Process output
        if result.returncode == 0:
            try:
                return jsonify(json.loads(result.stdout))
            except json.JSONDecodeError:
                return jsonify({"error": "Invalid JSON output", "output": result.stdout}), 500
        else:
            return jsonify({"error": result.stderr}), 500
    finally:
        # Clean up temp file
        if os.path.exists(f_path):
            os.unlink(f_path)

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8080)
