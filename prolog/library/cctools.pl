:- module(cctools, [clean_cont/2, print_cont/1]).

user:goal_expansion(empty_cc_frame(F), F='$cont$'(Ref,_,_,_,[])) :- clause(call_continuation([_|_]), _ , Ref).
clean_cont(call_continuation(F1), call_continuation(F2)) :- exclude_empty(F1, F2).

exclude_empty([],[]).
exclude_empty([F|Fs], Fs2) :- 
   (empty_cc_frame(F) -> Fs2=Fs3; Fs2=[F|Fs3]),
   exclude_empty(Fs,Fs3).

user:portray(clause(PI,Base,Line,PC)) :-
  format('{~q at ~w:~d @PC=~w}', [PI, Base, Line, PC]).

print_cont(Cont) :-
   convert(Cont,Cont1),
	print_term(Cont1,[right_margin(110)]), nl.

convert(X1,X2) :- compound(X1) -> X1 =.. [Head| Args], convert_compound(Head, Args, X2); X2=X1.
convert_compound('$cont$', [Clause, PC | Args1], frame(clause(PI,Base,Line,PC),Args2)) :- !,
  clause_property(Clause, file(File)),
  file_base_name(File, Base),
  clause_property(Clause, line_count(Line)),
  clause_property(Clause, predicate(PI)),
  maplist(convert, Args1, Args2).

convert_compound(Head, Args1, X2) :-
   maplist(convert,Args1,Args2),
   X2 =.. [Head |Args2].
