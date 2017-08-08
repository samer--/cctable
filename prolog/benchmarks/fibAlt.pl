% Tabling is also important for the memoisation it offers.
% The standard example is calculating Fibonacci numbers.
% NB. this differs from fib.pl in that the recursive calls
%  are made in the reverse order.


:- table fib/2.

case(fib(SIZE), ignore(fib(SIZE,_))).

fib(0, 1):-!.
fib(1, 1):-!.
fib(N,F):-N>1,N1 is N-1, N2 is N-2,fib(N2,F2),fib(N1,F1),F is F1+F2.
