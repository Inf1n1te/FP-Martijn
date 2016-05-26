% Sherry is the suject of this puzzle.
:- use_module(library(lists)).




/**
* first attempt
*/
% clue 1:
solvestand(sally,rockland,_,_) :- !, fail.
solvestand(_,_,thursday,peppermint) :- !, fail.
% clue 2:
solvestand(S1,C1,B,chocolatechip) :- stand(S1),city(C1),stand(S2),ice(I2),city(C3),ice(I3), is_set([S1,C1,S2,I2,C3,I3]),!, daysthree([A,B,C|_]), solvestand(S2,granite,A,I2), solvestand(gary,C3,C,I3).
% clue 3:
solvestand(tom,_,D,peanutbutter) :- !, not(D=tuesday).
% clue 4:
solvestand(S,_,wednesday,coffeebean) :- !, not(S=alice).
% clue 5:
solvestand(sally,C1,D1,I1) :- city(C1),ice(I1),stand(S2),ice(I2),is_set([C1,I1,S2,I2]), daystwo([D2,D1,_]), solvestand(S2,marsh,D2,I2).


stand(sally).
stand(gary).
stand(tom).
stand(alice).

city(rockland).
city(granite).
city(marsh).
city(boulder).

day(tuesday).
day(wednesday).
day(thursday).
day(friday).

daystwo([thursday, friday]).
daystwo([wednesday, thursday, friday]).
daystwo([tuesday, wednesday, thursday, friday]).

daysthree([wednesday, thursday, friday]).
daysthree([tuesday, wednesday, thursday, friday]).

ice(peanutbutter).
ice(peppermint).
ice(chocolatechip).
ice(coffeebean).

/**
* second attempt
*/


% clue 1:
stand(sally,rockland,_,_) :- !, fail.
stand(_,_,thursday,peppermint) :- !, fail.




stand(_,_,_,_).

tour([stand(_,_,tuesday,_), stand(_,_,wednesday,_), stand(_,_,thursday,_), stand(_,_,friday,_)]).

icecream([stand(S,C,D,I)], [S,C,I]) :- stand(S), city(C), ice(I), stand(S,C,D,I).
icecream([stand(S,C,D,I)|Tour], [S,C,I|Rest]) :- stand(S), city(C), ice(I), member(S,Rest), member(C,Rest), member(I,Rest) , stand(S,C,D,I), icecream(Tour, Rest).




