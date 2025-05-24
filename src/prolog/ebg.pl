% EBG for learning generalizations from examples
ebg(Goal, Head, Body) :-
    functor(Goal, F, N),
    functor(Head, F, N),
    ebg1(Goal, Head, Body, []).

ebg1(A, C, [C|G], G) :- operational(A), !, A.
ebg1((A,B), (GenA,GenB), NG, OG) :-
    ebg1(A, GenA, NG, G),
    ebg1(B, GenB, G, OG).
ebg1(A, GenA, NG, OG) :-
    clause(GenA, GenB),
    copy((GenA :- GenB), (A :- B)),
    ebg1(B, GenB, NG, OG).
ebg1(true, _, G, G).

% Built-in predicates are operational
operational(\==(_,_)).

% Utility for copying term structure with new variables
copy(Old, New) :-
    assert('$marker'(Old)),
    retract('$marker'(NN)),
    New = NN.
