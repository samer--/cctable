:- table d/1,e/1.

case(ping_pong(SIZE), count(d(_))).

% Two mutually recursive predicates:
d(X) :- e(Y), Y < SIZE, X is Y + 1.
d(0).

e(X) :- d(Y), Y < SIZE, X is Y + 1.
e(0).
