/***********************************************************
taken from "Extension Table Built-ins for Prolog", 
by Chang-Guan Fa & Suzanne W. Dietrich
Software Practive and Experience, Vol22, No.7, 573-597, 1992.
***********************************************************/
:- table nrev/2.

case(nrev(SIZE), verify(nrev(List,Result), reverse(List,Result))) :- numlist(1,SIZE,List).

nrev([],[]).
nrev([X|Rest], Result) :-
  nrev(Rest,L),
  myappend(L,[X],Result).

myappend([],L,L).
myappend([X|L1], L2, [X|L3]) :-
  myappend(L1,L2,L3).
