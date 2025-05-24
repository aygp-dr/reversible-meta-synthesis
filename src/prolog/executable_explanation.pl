% Representation of an explanation in Prolog clauses
% Each etree/2 predicate represents a node in the explanation tree

% Example of an executable explanation for the append program
etree(1, append([], L, L)).
etree(2, append([H|T1], L2, [H|T3])) :- etree(3, append(T1, L2, T3)).

% Cut and paste operation for explanations based on composability
etree(Id, Goal) :-
    composability(Goal, C),
    DECOMP_FORCE >= C,  % Global variable to control decomposition depth
    clause(etree(SId, Goal), Body),
    nonvar(SId),
    Id \== SId,
    Body.
