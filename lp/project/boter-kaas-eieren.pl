tictactoe(_,B,B) :- win(B), printb(B).
tictactoe(X,B,W) :- move(X,B,Bn), other(X,O), tictactoe(O,Bn,W).

emptyb(b([0,0,0],[0,0,0],[0,0,0])).

other(x,o).
other(o,x).

move(X,b([0,A1,A2],B,C),b([X,A1,A2],B,C)). 
move(X,b([A0,0,A2],B,C),b([A0,X,A2],B,C)). 
move(X,b([A0,A1,0],B,C),b([A0,A1,X],B,C)). 
move(X,b(A,[0,B1,B2],C),b(A,[X,B1,B2],C)). 
move(X,b(A,[B0,0,B2],C),b(A,[B0,X,B2],C)). 
move(X,b(A,[B0,B1,0],C),b(A,[B0,B1,X],C)). 
move(X,b(A,B,[0,C1,C2]),b(A,B,[X,C1,C2])). 
move(X,b(A,B,[C0,0,C2]),b(A,B,[C0,X,C2])). 
move(X,b(A,B,[C0,C1,0]),b(A,B,[C0,C1,X])).


same([_]).
same([X,X|T]) :- other(X,_), same([X|T]).
win(b(A,B,C)) :- same(A); same(B); same(C).
win(b([X,_,_],[_,X,_],[_,_,X])) :- other(X,_).
win(b([_,_,X],[_,X,_],[X,_,_])) :- other(X,_).
win(b([],[],[])) :- fail.
win(b([A|As],[B|Bs],[C|Cs])) :- other(A,_), A=B, B=C, win(b(As,Bs,Cs)). 

printbb([A]) :- printb(A).
printbb([A|As]) :- printb(A), printbb(As).

printb(b([],[],[])).
printb(b([A],[B],[C])) :- write("+-+-+-+\n"),write("|"),write(A),write("|"),write(B),write("|"),write(C),write("|\n"),write("+-+-+-+\n").
printb(b([A|As],[B|Bs],[C|Cs])) :- write("+-+-+-+\n"),write("|"),write(A),write("|"),write(B),write("|"),write(C),write("|\n"),printb(b(As,Bs,Cs)).
