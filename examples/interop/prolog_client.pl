:- consult('../../src/prolog/interop_bridge.pl').

% Example of executing a program in another language
example_execute_in_hy :-
    % Define the append program
    Program = [
        ([append, [], *l, *l] :- []),
        ([append, [*x | *l1], *l2, [*x|*l3]] :- [[append, *l1, *l2, *l3]])
    ],
    % Define a query
    Query = [[[append, [/a, /b], [/c, /d], *ans]]],
    
    % Export to JSON
    export_program(Program, ProgramJSON),
    export_program(Query, QueryJSON),
    
    % Construct input for Hy implementation
    Input = [clauses=ProgramJSON, queries=QueryJSON],
    
    % Call Hy implementation
    call_external(hy, execute, Input, Output),
    
    % Display result
    write('Result from Hy implementation: '), write(Output), nl.

% Example of building an explanation in another language
example_explanation_in_clojure :-
    % Define the append program
    Program = [
        ([append, [], *l, *l] :- []),
        ([append, [*x | *l1], *l2, [*x|*l3]] :- [[append, *l1, *l2, *l3]])
    ],
    % Define a goal
    Goal = [append, [/a], [/b], [/a, /b]],
    
    % Export to JSON
    export_program(Program, ProgramJSON),
    term_to_json(Goal, GoalJSON),
    
    % Construct input for Clojure implementation
    Input = [goal=GoalJSON, clauses=ProgramJSON],
    
    % Call Clojure implementation
    call_external(clojure, build_explanation, Input, Output),
    
    % Import the explanation
    import_explanation(Output, Explanation),
    
    % Display result
    write('Explanation built by Clojure: '), nl,
    write_explanation(Explanation, 0).

% Helper to display an explanation tree
write_explanation(expl_node(Goal, Children), Indent) :-
    write_indent(Indent),
    write(Goal), nl,
    NewIndent is Indent + 2,
    maplist(write_explanation_child(NewIndent), Children).

write_explanation_child(Indent, Child) :-
    write_explanation(Child, Indent).

write_indent(0) :- !.
write_indent(N) :-
    write(' '),
    N1 is N - 1,
    write_indent(N1).

% Run examples
main :-
    example_execute_in_hy,
    example_explanation_in_clojure.

:- main.
