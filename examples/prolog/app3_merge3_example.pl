:- consult('../../src/prolog/reversible_interpreter.pl').
:- consult('../../src/prolog/ebg.pl').
:- consult('../../src/prolog/composability.pl').
:- consult('../../src/prolog/executable_explanation.pl').

% Precedent program: app3
example_app3 :-
    prolog([([append, [], *x, *x] :- []),
            ([append, [*x|*l1], *l2, [*x|*l3]] :- [[append, *l1, *l2, *l3]]),
            ([app3, *x, *y, *z, *a] :- [[append, *x, *y, *aa], [append, *aa, *z, *a]])],
            [[[app3, [/a], [/b], [/c], [/a, /b, /c]]]],
            Value),
    write('Value: '), write(Value), nl.

% Synthesis of merge3 using clause-level chunks
example_synthesize_merge3 :-
    % Set global variable to decompose at clause level
    nb_setval(DECOMP_FORCE, 1),
    
    % First create explanation from app3
    prolog([([append, [], *x, *x] :- []),
            ([append, [*x|*l1], *l2, [*x|*l3]] :- [[append, *l1, *l2, *l3]]),
            ([app3, *x, *y, *z, *a] :- [[append, *x, *y, *aa], [append, *aa, *z, *a]])],
            [[[app3, [/a], [/b], [/c], [/a, /b, /c]]]],
            _),
    
    % Then synthesize merge3 using the explanation
    etree(1,
          prolog([([merge, [], *x, *x] :- []),
                  ([merge, *x, [], *x] :- []),
                  ([merge, [*x|*l1], [*y|*l2], [*x|*l3]] :- [[=<, *x, *y], [merge, *l1, [*y|*l2], *l3]]),
                  ([merge, [*x|*l1], [*y|*l2], [*y|*l3]] :- [[>, *x, *y], [merge, [*x|*l1], *l2, *l3]])
                  | Clauses],
                 [[[merge3, [/3], [/1], [/2], [/1, /2, /3]]]],
                 [[]])),
    write('Synthesized merge3 Clauses: '), write(Clauses), nl.
