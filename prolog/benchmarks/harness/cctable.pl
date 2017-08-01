:- use_module('../../cctable').
:- use_module('../../ccmacros').
:- prompt(_,''), load_files(user,[stream(user_input)]).

main(Show) :-
   case(Name, Case),
   run_tabled(run_case(Case,Result,Time1)), garbage_collect,
   run_tabled(run_case(Case,Result,Time2)),
   format('~w: ~`.t time = ~w ms~60|\n', [Name, [Time1,Time2]]),
   ( Show=1 -> format('~w: result = ~w\n', [Name, Result])
   ; true),
   halt.

run_case(call(Goal,Res), answer(Res), Time) :-
   T1 is cputime, call(Goal), T2 is cputime, Time is round(1000*(T2 - T1)).

run_case(count(Goal), solutions(N), Time) :-
   run_case(call(findall(t,Goal,R), R), answer(Solns), Time),
   length(Solns,N).

