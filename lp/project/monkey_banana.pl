

init(state(atdoor, onfloor, atwindow, hasnot)).

%walk(atdoor, atmiddle).
%walk(atmiddle, atwindow).
%walk(atwindow, atmiddle).
%walk(atmiddle, atdoor).

%push(X,Y) :- walk(X,Y).

%climb.
%grasp.

move(state(atmiddle, onbox, atmiddle, hasnot), grasp, state(atmiddle, onbox, atmiddle, has)).
move(state(Pos, onfloor, Pos, Has), climb, state(Pos, onbox, Pos, Has)).
move(state(X1, onfloor, X1, Has), push(X1,X2), state(X2, onfloor, X2, Has)).
move(state(X1, onfloor, Pos, Has), walk(X1,X2), state(X2, onfloor, Pos, Has)).

solve(state(_,_,_,has),[]).
solve(X,[Y|L]) :- move(X,Y,Z), solve(Z,L).

