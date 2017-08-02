report(Show, Name, Time, Result) :-
   format('~w: ~`.t time = ~w ms~60|\n', [Name, Time]),
   ( Show=1 -> format('~w: result = ~w\n', [Name, Result])
   ; true).

run_case(call(Goal,Res), answer(Res), Time) :-
   T1 is cputime, call(Goal), T2 is cputime, Time is round(1000*(T2 - T1)).

run_case(count(Goal), solutions(N), Time) :-
   run_case(call(findall(t,Goal,R), R), answer(Solns), Time),
   length(Solns,N).
