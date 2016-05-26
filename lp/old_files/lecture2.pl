fib(0,1) :- !.
fib(1,1) :- !.
fib(N,Res) :- N1 is N-1, N2 is N-2, fib(N1,Res1), fib(N2,Res2), Res is Res1 + Res2.

fib2(0,1) :- !.
fib2(N,Res) :- fib3(1,1,N,Res).

fib3(_,X2,1,X2) :- !.
fib3(X1,X2,N,Res) :- X3 is X1+X2, N1 is N-1, fib3(X2,X3,N1,Res).

member(X,[X|_]) :- !.
member(X,[_|Xs]) :- member(X,Xs).

last([X],X) :- !.
last([_|Xs],Y) :- last(Xs,Y).

quicksort([],[]) :- !.
quicksort([X|Xs],Y) :- split(X,Xs,LT,GT), quicksort(LT,Z1), quicksort(GT,Z2), append(Z1,[X|Z2],Y).

split(_,[],[],[]) :- !.
split(N,[X|Xs],[X|LT],GT) :- X=<N, !, split(N,Xs,LT,GT).
split(N,[X|Xs],LT,[X|GT]) :- split(N,Xs,LT,GT).

slowsort(X,Y) :- perm(X,Y), sorted(Y).

sorted([]).
sorted([_]).
sorted([X,Y|Z]) :- X=<Y, sorted([Y|Z]).

perm([],[]).
perm([X|Y],Z) :- perm(Y,A),insert(X,A,Z).

insert(X,A,[X|A]).
insert(X,[K|S],[K|R]) :- insert(X,S,R).

t(_,nil,nil).
t(X,Y,Z) :- integer(X), Y = t(_,_,_), Z = t(_,_,_).

tsum(nil, 0).
tsum(t(X,Y,Z), N) :- tsum(Y,A), tsum(Z,B), N is X + A + B.

maketree(X) :-
	T1 = tree(nil,2,nil),
	T2 = tree(nil,5,nil),
	T3 = tree(T1,11,T2),
	X = tree(T3,21,T3).

maketree2(X) :- X = tree(tree(tree(nil,2,nil),11,tree(nil,5,nil)),21,tree(tree(nil,2,nil),11,tree(nil,5,nil))).

sum(nil,0).
sum(tree(T1,N,T2),S) :- sum(T1,S1),sum(T2,S2),S is N+S1+S2.

go(Y) :- maketree(X),sum(X,Y).

p(X).
