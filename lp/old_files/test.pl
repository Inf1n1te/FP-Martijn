p(a).
p(b).
p(c).

q(a).
q(d).

s(X):-q(X),q(a).

t(X):-p(X).
t(X):-q(X).

r(X).
r(a).
r(b).
r(X,a).
r(b,X).
r(X,Y).
