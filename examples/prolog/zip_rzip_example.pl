:- consult('../../src/prolog/reversible_interpreter.pl').
:- consult('../../src/prolog/ebg.pl').
:- consult('../../src/prolog/composability.pl').
:- consult('../../src/prolog/executable_explanation.pl').

% Precedent program: zip
example_zip :-
    prolog([([zip, [], *l, *l] :- []),
            ([zip, [*x | *l1], [*y | *l2], [*x, *y | *l3]] :- [[zip, *l1, *l2, *l3]])],
            [[[zip, [/x, /y], [/z, /u], *ans]]],
            Value),
    write('Value: '), write(Value), nl.

% Synthesis of rzip using clause-structure-level chunks
example_synthesize_rzip :-
    % Set global variable to decompose at clause-structure level
    nb_setval(DECOMP_FORCE, 2),
    
    % First create explanation from zip
    prolog([([zip, [], *l, *l] :- []),
            ([zip, [*x | *l1], [*y | *l2], [*x, *y | *l3]] :- [[zip, *l1, *l2, *l3]])],
            [[[zip, [/x, /y], [/z, /u], *ans]]],
            _),
    
    % Then synthesize rzip using the explanation
    etree(1,
          prolog(Clauses,
                 [[[rzip, [/1, /2], [/3, /4], [/3, /1, /4, /2]]]],
                 [[]])),
    write('Synthesized rzip Clauses: '), write(Clauses), nl.
