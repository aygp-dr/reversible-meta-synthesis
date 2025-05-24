:- consult('../../src/prolog/reversible_interpreter.pl').

% Example of using the reversible interpreter to run append
example_run_append :-
    prolog([([append, [], *l, *l] :- []),
           ([append, [*x | *l1], *l2, [*x|*l3]] :- [[append, *l1, *l2, *l3]])],
           [[[append, [/x], [/y], *ans]]],
           Value),
    write('Value: '), write(Value), nl.

% Example of program synthesis for append
example_synthesize_append :-
    prolog(Clauses,
           [[[append, [/a], [/b], [/a, /b]]]],
           [[]]),
    write('Synthesized Clauses: '), write(Clauses), nl.
