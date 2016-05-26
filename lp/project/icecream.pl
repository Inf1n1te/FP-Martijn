:- use_module(library(lists)).

stand(sally).
stand(gary).
stand(tom).
stand(alice).

town(rockland).
town(granite).
town(marsh).
town(boulder).

day(tuesday).
day(wednesday).
day(thursday).
day(friday).

flavor(peanutbutter).
flavor(peppermint).
flavor(chocolatechip).
flavor(coffeebean).

% stand(Day,Stand,Town,Flavor).
shop(_,sally,rockland,_):- !, fail.
shop(thursday,_,_,peppermint):- !, fail.
shop(Day,tom,_,peanutbutter):- !, not(Day=tuesday).
shop(wednesday,Stand,_,coffeebean):- !, not(Stand=alice).

solve([shop(tuesday,S0,T0,F0),shop(wednesday,S1,T1,F1),shop(thursday,S2,T2,F2),shop(friday,S3,T3,F3)]):-
	stand(S0),stand(S1),stand(S2),stand(S3),
	town(T0),town(T1),town(T2),town(T3),
	flavor(F0),flavor(F1),flavor(F2),flavor(F3),
	is_set([S0,S1,S2,S3]),
	is_set([T0,T1,T2,T3]),
	is_set([F0,F1,F2,F3]),
	ice_check([shop(tuesday,S0,T0,F0),shop(wednesday,S1,T1,F1),shop(thursday,S2,T2,F2),shop(friday,S3,T3,F3)]).

ice_check(L):-
	nextto(
		shop(_,_,marsh,_),
		shop(_,sally,_,_),
	L),
	nextto(
		shop(_,gary,_,_),
		shop(_,_,granite,_),
	L),
	nextto(
		shop(_,_,granite,_),
		shop(_,_,_,chocolatechip),
	L).
