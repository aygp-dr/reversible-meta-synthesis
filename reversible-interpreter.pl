% Reversible Meta-Interpreter
% Based on Numao and Ma's paper on "Inductive Program Synthesis by Using a Reversible Meta-Interpreter"

% Define operators for variables and constants
:- op(250, fx, '*'). % * is prefixed to variables
:- op(250, fx, '/'). % / is prefixed to constants

% Main interpreter predicate
% prolog(Clauses, Queries, Value): Answer Queries one by one by calling goalseq
prolog(_, [], []).
prolog(Clauses, [Q | Qs], [NewEnv | RestEnv]) :-
    goalseq(Q, [], NewEnv, Clauses),
    prolog(Clauses, Qs, RestEnv).

% goalseq(Query, OldEnv, NewEnv, Clauses):
% Answer a Query, which is a sequence of goals.
% Values of variables are retained in a difference-list between NewEnv and OldEnv.
goalseq([], Env, Env, _Clauses).
goalseq([Goal | GRest], Env, NewEnv, Clauses) :-
    head(Goal, GG, Env, E),
    goal(GG, Clauses),
    goalseq(GRest, E, NewEnv, Clauses).

% goal(Goal, Clauses): Answer a Goal.
goal(Goal, Clauses) :-
    search(Clauses, (Head :- Body)),
    head(Head, Goal, [], NextEnv),
    goalseq(Body, NextEnv, _, Clauses).

% search(Clauses, Clause): Search a Clause in the database Clauses.
search([Clause | _], Clause).
search([_ | Clauses], C) :- search(Clauses, C).

% head(Goal with variables, Goal without variables, OldEnv, NewEnv):
% Put/Get value of variables in a Goal.
head([Pred | XA], [Pred | YA], OEnv, NEnv) :-
    head1(XA, YA, OEnv, NEnv).

head1([X | XRest], [Y | YRest], OEnv, NEnv) :-
    bind(X, Y, OEnv, Env),
    head1(XRest, YRest, Env, NEnv).
head1([], [], Env, Env).

% bind(Term with variables, Term without variables, OldEnv, NewEnv):
% Put/Get value of variables in a Term.
bind(/X, /X, Env, Env).
bind([], [], Env, Env).
bind(*Var, Val, Env, NewEnv) :- fetch(*Var, Val, Env, NewEnv).
bind([X|XRest], [Val|ValRest], Env, NewEnv) :-
    bind(X, Val, Env, New1),
    bind(XRest, ValRest, New1, NewEnv).

% fetch(Variable, Value, OldEnv, NewEnv): Put/Get Value of a Variable.
fetch(Var, Val, Env, NewEnv) :- fetch1(Var, Val, Env, Env, NewEnv).

fetch1(Var, Val, [], Env, [(Var, Val) | Env]).
fetch1(Var, Val, [(Var, Val) | _Rest], Env, Env).
fetch1(Var1, Val, [(Var2, _) | EnvRest], Env, NewEnv) :-
    Var1 \== Var2,
    fetch1(Var1, Val, EnvRest, Env, NewEnv).

% Test cases
test_append :-
    % Define append program
    AppendProgram = [
        ([append, [], *l, *l] :- []),
        ([append, [*x | *l1], *l2, [*x|*l3]] :- [[append, *l1, *l2, *l3]])
    ],
    
    % Test execution mode - running append
    write('Test execution: append([a, b], [c, d], Result)'), nl,
    prolog(AppendProgram, 
           [[[append, [/a, /b], [/c, /d], *result]]],
           Value),
    write('Result: '), write(Value), nl, nl,
    
    % Test synthesis mode - creating append from examples
    write('Test synthesis: Creating append from examples'), nl,
    prolog(SynthesizedClauses,
           [[[append, [], [/x, /y], [/x, /y]]],
            [[append, [/a], [/b], [/a, /b]]],
            [[append, [/a, /b], [/c], [/a, /b, /c]]]],
           [[], [], []]),
    write('Synthesized program:'), nl,
    write_clauses(SynthesizedClauses).

% Helper predicate to write clauses
write_clauses([]).
write_clauses([Clause | Rest]) :-
    write('  '), write(Clause), nl,
    write_clauses(Rest).

% Test reverse
test_reverse :-
    % Define reverse program using accumulator
    ReverseProgram = [
        ([reverse, *xs, *ys] :- [[rev_acc, *xs, [], *ys]]),
        ([rev_acc, [], *acc, *acc] :- []),
        ([rev_acc, [*x | *xs], *acc, *ys] :- [[rev_acc, *xs, [*x | *acc], *ys]])
    ],
    
    % Test execution mode
    write('Test execution: reverse([a, b, c], Result)'), nl,
    prolog(ReverseProgram, 
           [[[reverse, [/a, /b, /c], *result]]],
           Value),
    write('Result: '), write(Value), nl.