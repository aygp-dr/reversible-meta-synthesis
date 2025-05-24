:- use_module(library(plunit)).

% Load the implementation files
:- consult('../../src/prolog/reversible_interpreter.pl').
:- consult('../../src/prolog/ebg.pl').
:- consult('../../src/prolog/composability.pl').
:- consult('../../src/prolog/executable_explanation.pl').

% Test suite for the reversible interpreter
:- begin_tests(reversible_interpreter).

test(basic_append_execution) :-
    prolog([([append, [], *l, *l] :- []),
            ([append, [*x | *l1], *l2, [*x|*l3]] :- [[append, *l1, *l2, *l3]])],
           [[[append, [/a, /b], [/c, /d], *ans]]],
           Value),
    Value = [[(*ans, [/a, /b, /c, /d])]].

test(basic_append_synthesis) :-
    prolog(Clauses,
           [[[append, [], [/a, /b], [/a, /b]]],
            [[append, [/c], [/d], [/c, /d]]]],
           [[], []]),
    member(([append, [], *l, *l] :- []), Clauses),
    member(([append, [*x | *l1], *l2, [*x|*l3]] :- [[append, *l1, *l2, *l3]]), Clauses).

:- end_tests(reversible_interpreter).

% Test suite for explanation-based learning
:- begin_tests(ebg).

test(ebg_basic) :-
    % Test that EBG can generalize a simple append example
    Goal = prolog([([append, [], *l, *l] :- []),
                  ([append, [*x | *l1], *l2, [*x|*l3]] :- [[append, *l1, *l2, *l3]])],
                 [[[append, [/a, /b], [/c, /d], *ans]]],
                 [[(*ans, [/a, /b, /c, /d])]]),
    ebg(Goal, Head, _Body),
    % Check that the generalized head contains the right predicate
    Head = prolog(_, _, _).

:- end_tests(ebg).

% Execute the tests
:- run_tests.
:- halt.
