report(Show, Name, Time, Result) :-
   format('~w: ~`.t time = ~w ms~60|', [Name, Time]),
   (Show=1 -> format(' -- ~w\n', [Result]); nl).

run_case(call(Goal,Res), answer(Res), Time)    :- time(Goal, Time).
run_case(ignore(Goal), ignored, Time)          :- time(Goal, Time).
run_case(verify(Goal,Check), correct(R), Time) :- time(Goal, Time), (call(Check) -> R = true; R=false).
run_case(count(Goal), solutions(N), Time) :-
   run_case(call(findall(t,Goal,R), R), answer(Solns), Time),
   length(Solns,N).

time(Goal,Time) :-
   T1 is cputime, call(Goal),
   T2 is cputime, Time is round(1000*(T2 - T1)).

run_case_limited(Case, Result, Time) :-
   run_case_limited(180, Case, Result, Time).
run_case_limited(Limit, Case, Result, Time) :-
   catch( call_with_time_limit(Limit, run_tabled(run_case(Case,Result,Time))),
          time_limit_exceeded, (Time=inf, Result=timeout(Limit))).
