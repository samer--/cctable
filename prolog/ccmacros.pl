:- module(ccmacros, [op(1150,fx,table), head_worker/2]).
/** <module> Term expansions to support tabling

This module implements a shallow program transformation to support
tabling. Predicates decalared `tabled` are renamed (by a appending
a '#' to their given name) and the original predicate name defined
as a metacall of the renamed predicate via cctable/1, which is 
assumed to be available in the module where the tabled precicate
is defined.
*/

:- op(1150,fx,table).

%% table(PredSpecfiers) is det
%  Declare predicates in PredSpecifiers (a comma separated list of Name/Arity
%  predicate specifier) as tabled.
system:term_expansion((:- table(Specs)), Clauses) :- 
   foldl_clist(expand_cctab, Specs, Clauses, []).

foldl_clist(P,(A,B)) --> !, call(P,A), foldl_clist(P,B).
foldl_clist(P,A) --> call(P,A).

expand_cctab(Name//Arity) --> !, 
   {A2 is Arity+2}, 
   expand_cctab(Name/A2).
expand_cctab(Name/Arity) --> 
   { functor(Head, Name, Arity), head_worker(Head, Worker)},
   [ (:- discontiguous('$cctabled'/1))
   , '$cctabled'(Head)
   , (Head :- cctabled(Worker))
   ]. 

prolog:rename_predicate(M:Head, M:Worker) :-
   '$flushed_predicate'(M:'$cctabled'(_)),
   call(M:'$cctabled'(Head)), !,
   head_worker(Head, Worker).

head_worker(Head, Worker) :-
   Head   =.. [H|As], atom_concat(H,'#',W),
   Worker =.. [W|As].
