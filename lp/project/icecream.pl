% Logic Programming Project by:
% Martijn Verkleij		- s1466895
% Tim Kerkhoven			- s1375253
%
% Run go(X) to start the program.

% Solver function, argument is an ordered list (ordered by day of the week) of the four shops
solve([shop(tuesday,S0,T0,F0),shop(wednesday,S1,T1,F1),shop(thursday,S2,T2,F2),shop(friday,S3,T3,F3)]):-
	% Binding the variables to the appropriate values using permutations:
	permutation([sally,gary,tom,alice],[S0,S1,S2,S3]),
	permutation([rockland,granite,marsh,boulder],[T0,T1,T2,T3]),
	permutation([peanutbutter,peppermint,chocolatechip,coffeebean],[F0,F1,F2,F3]),
	% Checking the constraints on individuals shops (clues# 1, 3, 4):
	shop_check([shop(tuesday,S0,T0,F0),shop(wednesday,S1,T1,F1),shop(thursday,S2,T2,F2),shop(friday,S3,T3,F3)]),
	% Checking the order constraints (clues# 2, 5):
	order_check([shop(tuesday,S0,T0,F0),shop(wednesday,S1,T1,F1),shop(thursday,S2,T2,F2),shop(friday,S3,T3,F3)]).

% Checks for (non)membership of elements in the list of shops:
shop_check(L):-
	member(shop(Day,tom,_,peanutbutter),L),not(Day=tuesday),		% Clue# 3
	member(shop(wednesday,Stand,_,coffeebean),L),not(Stand=alice),	% Clue# 4
	not(member(shop(_,sally,rockland,_),L)),						% Clue# 1
	not(member(shop(thursday,_,_,peppermint),L)).					% Clue# 1

% Checks the order of shops in the list of shops:
order_check(L):-
	nextto(							% Clue# 5
		shop(_,_,marsh,_),
		shop(_,sally,_,_),
	L),
	nextto(							% Clue# 2
		shop(_,gary,_,_),
		shop(_,_,granite,_),
	L),
	nextto(							% Clue# 2
		shop(_,_,granite,_),
		shop(_,_,_,chocolatechip),
	L).

% Run the program.
go(X):- solve(X).
