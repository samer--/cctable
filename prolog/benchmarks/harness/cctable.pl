:- use_module('../../ccmacros').
:- current_prolog_flag(argv, [Imp,File|_]),
   atom_concat('../../', Imp, Module),
   use_module(Module),
   load_files(File,[]).
:- consult(swipl_common).

main(Show) :-
   case(Name, Case),
   run_case_limited(90, Case, _, Time1), !, garbage_collect,
   run_case_limited(240, Case, Result, Time2),
   current_prolog_flag(argv, [Imp,_]),
   report(Show, Imp, Name, [Time1,Time2], Result).
