:-dynamic(person/2).
:- initialization(once(((main ; true), halt))).

addUser :- write('Please enter the following details. End each expression with a .\nname: '), read(X), write('number: '), read(Y), assertz(person(X,Y)).

getNumber :- write('Enter the name of the person you want to know the number of :\n'),read(X),person(X,Y),write(Y),nl,fail.
getNumber.
changeNumber :- write('Name of person: \n'),read(X),person(X,Y),write('Number: '),write(Y),write('\nEnter new number: '),read(Z),retract(person(X,Y)),assertz(person(X,Z)).
deletePerson :- write('Name of the person: \n'),read(X),person(X,Y),write('Delete person: '),write(X),write(' with number: '),write(Y),write('?\n'),read(Q),(Q='y';Q='yes'),retract(person(X,Y)).

list :- person(X,Y), write('person: '), write(X), write(' nr: '), write(Y), write('\n'), fail.
list.

exit :- tell('database.pl'), write(':- dynamic(person/2).\n'), person(X,Y), write('person('), write(X), write(','), write(Y), write(').\n'), fail.
exit :- told, tell(user).

load :- consult('database.pl').

main :- load, menu.

menu :- write('\n 1: Add User\n 2: Get Number\n 3: Change Number\n 4: Delete Person\n 5: List All Entries\n 6: Save & Exit\n Choose: '), nl, read(X), menu(X).
menu(1) :- addUser, menu.
menu(2) :- getNumber, menu.
menu(3) :- changeNumber, menu.
menu(4) :- deletePerson, menu.
menu(5) :- list, menu.
menu(6) :- exit.
menu(_) :- menu.