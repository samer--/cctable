:- module(cctable_terms, [numbervars_copy/2]).

numbervars_copy(Head, VC) :-
   copy_term_nat(Head, VC),
   numbervars(VC, 0, _).

