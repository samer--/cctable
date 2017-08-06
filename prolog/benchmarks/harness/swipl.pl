:- use_module(library(tabling)).
:- consult(swipl_common).
:- current_prolog_flag(argv, [File|_]),
   load_files(File,[]).

main(Show) :-
   case(Name, Case),
   run_case(Case,_,Time1), abolish_all_tables,
   run_case(Case,Result,Time2),
   report(Show, Name, [Time1,Time2], Result).

