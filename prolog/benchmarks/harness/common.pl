report(0, _Imp, Name, Time, _Result) :- format('~w: ~`.t time = ~w ms~60|\n', [Name, Time]).
report(1, _Imp, Name, Time, Result) :- format('~w: ~`.t time = ~w ms~60| -- ~w\n', [Name, Time, Result]).
report(2, Imp, Name, Times, _Result) :- 
   last_time(Times,T),
   format('imp_case_time(~q, ~q, ~q).\n', [Imp, Name, T]).


last_time(T,T) :- number(T), !.
last_time(Ts,T) :- append(_,[T],Ts).
