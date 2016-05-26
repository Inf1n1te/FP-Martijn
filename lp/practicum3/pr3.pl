:- use_module(library(clpfd)).

test1(X,Y):-
	X in 0..10,
	Y in 4..8,
	X #> Y.
