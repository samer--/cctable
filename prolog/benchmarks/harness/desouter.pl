:- current_prolog_flag(argv, [Imp,File|_]),
   member(Imp-Module, [pl-'../../../tabling_library/tabling', plc-library(tabling)]),
   use_module(Module),
   load_files(File,[]).
:- consult(swipl_common).

main(Show) :-
   case(Name, Case),
   run_case(Case, _, Time1), abolish_all_tables,
   run_case(Case, Result, Time2),
   current_prolog_flag(argv, [Imp,_|_]),
   report(Show, desouter-Imp, Name, [Time1,Time2], Result).
