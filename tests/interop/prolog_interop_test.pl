:- module(prolog_interop_test, [run_tests/0]).

:- use_module(library(plunit)).
:- consult('../../src/prolog/interop_bridge.pl').

:- begin_tests(interop_bridge).

test(export_import_program) :-
    % Define a program
    Program = [
        ([append, [], *l, *l] :- []),
        ([append, [*x | *l1], *l2, [*x|*l3]] :- [[append, *l1, *l2, *l3]])
    ],
    % Export to JSON
    export_program(Program, JSON),
    % Import back
    import_program(JSON, ImportedProgram),
    % Should be equal
    Program = ImportedProgram.

test(export_import_explanation) :-
    % Define an explanation tree
    Explanation = expl_node(
        [append, [/a], [/b], [/a, /b]],
        [expl_node([append, [], [/b], [/b]], [])]
    ),
    % Export to JSON
    export_explanation(Explanation, JSON),
    % Import back
    import_explanation(JSON, ImportedExplanation),
    % Should be equal
    Explanation = ImportedExplanation.

:- end_tests(interop_bridge).

run_tests :-
    run_tests(interop_bridge).
