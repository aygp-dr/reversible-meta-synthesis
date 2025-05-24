% Example of a higher-order synthesis goal
% Synthesize map function from examples

% Desired behavior
example(map(increment, [1, 2, 3], [2, 3, 4])).
example(map(square, [1, 2, 3], [1, 4, 9])).
example(map(double, [1, 2, 3], [2, 4, 6])).

% Helper functions
increment(X, Y) :- Y is X + 1.
square(X, Y) :- Y is X * X.
double(X, Y) :- Y is X * 2.

% Synthesized map predicate would be:
% map(_, [], []).
% map(F, [H1|T1], [H2|T2]) :- call(F, H1, H2), map(F, T1, T2).
