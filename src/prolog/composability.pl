% Composability definitions from the paper
composability(prolog(_,_,_), 1).
composability(goalseq(_,_,_,_), 3).
composability(goal(_,_), 1).
composability(search(_,_), 1).
composability(head(_,_,_,_), 4).
composability(head1(_,_,_,_), 4).
composability(bind(_,_,_,_), 4).
composability(fetch(_,_,_,_), 2).
composability(fetch1(_,_,_,_,_), 3).
