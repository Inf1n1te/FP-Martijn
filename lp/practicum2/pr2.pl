istree(nil).
istree(t(L,_,R)):-istree(L),istree(R).

max(t(_,N,nil),N):-!.
max(t(_,_,R),M):-max(R,M).

min(t(nil,N,_),N):-!.
min(t(L,_,_),M):-min(L,M).

issorted(t(nil,_,nil)):-!.
issorted(t(nil,N,R)):-issorted(R),!,min(R,X),X>=N.
issorted(t(L,N,nil)):-issorted(L),!,max(L,X),X=<N.
issorted(t(L,N,R)):-issorted(L),issorted(R),max(L,X),min(R,Y),X=<N,Y>=N.

find(t(L,N,_),I,S):-I<N,!,find(L,I,S).
find(t(_,N,R),I,S):-I>N,!,find(R,I,S).
find(t(L,N,R),I,S):-!,I==N,S=t(L,N,R).

insert(nil,I,t(nil,I,nil)):-!.
insert(t(L,N,R),I,S):-I>=N,!,insert(R,I,Z),S=t(L,N,Z);I=<N,!,insert(L,I,Z),S=t(Z,N,R).

delete(t(L,N,R),I,S):-I>N,!,delete(R,I,Z),S=t(L,N,Z);I<N,!,delete(L,I,Z),S=t(Z,N,R).
delete(t(L,N,nil),I,S):-I==N,S=L,!.
delete(t(L,N,R),I,S):-I==N,!,min(R,RN),delete(R,RN,Y),S=t(L,RN,Y).

deleteall(T,I,S):-find(T,I,_),delete(T,I,Z),!,deleteall(Z,I,S).
deleteall(T,_,T).

listtree([],nil).
listtree([L|LS],T):-listtree(LS,S),insert(S,L,T).

treelist(nil,[]):-!.
treelist(T,[X|M]):-min(T,X),delete(T,X,S),treelist(S,M).

treelistunique(nil,[]):-!.
treelistunique(T,[X|M]):-min(T,X),deleteall(T,X,S),treelistunique(S,M).

treesort(L1,L2):-listtree(L1,T),treelist(T,L2).
treesortunique(L1,L2):-listtree(L1,T),treelistunique(T,L2).
