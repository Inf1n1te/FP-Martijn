% Example prolog database. Analogous to program1 in Level3.hs

p(X,Y):-r(b),s(X),t(X).
q(Y,Z):-p(Z,Y),t(Y).
r(a).
r(b).
r(c).
s(Z):-r(Z).
s(d).
t(b).
t(d).
