:- use_module(library(tabling)).
:- consult(swipl_common).
:- current_prolog_flag(argv, [File|_]),
   load_files(File,[]).

main(Show) :-
   case(Name, Case),
   run_case(Case,Result,Time),
   report(Show, Name, Time, Result).

