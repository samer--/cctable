:- table c/1.

case(shuttle(SIZE), count(c(_))).

c(X) :- c(Y), 0 =< Y, Y < SIZE, X is -Y-1.
c(X) :- c(Y), -SIZE < Y, Y =< 0, X is -Y+1.
c(0).
