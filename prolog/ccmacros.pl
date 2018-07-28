:- module(ccmacros, [op(1150,fx,table), head_worker/2]).
/** <module> Term expansions to support tabling

This module implements a shallow program transformation to support
tabling. Predicates decalared `tabled` are renamed (by a appending
a '#' to their given name) and the original predicate name defined
as a metacall of the renamed predicate via cctable/1, which is
assumed to be available in the module where the tabled precicate
is defined.

Alternatives, tabled predicates declared as
==
   :- table pred(_,_,M).
==
where the last argument is nonvar are transformed for use with
mode directed tabling.
*/

:- op(1150,fx,table).

%% table(PredSpecfiers) is det
%  Declare predicates in PredSpecifiers (a comma separated list of Name/Arity
%  predicate specifier) as tabled.

%! head_worker(+Head, -Worker) is det.
%  Rename the head functor of any term by appending '#'. This is
%  used to rename tabled predicates.
head_worker(Head, Worker) :-
   Head   =.. [H|As], atom_concat(H,'#',W),
   Worker =.. [W|As].

foldl_clist(P,(A,B)) --> !, call(P,A), foldl_clist(P,B).
foldl_clist(P,A) --> call(P,A).

decl(F//A) --> !, {A2 is A+2}, decl(F/A2).
decl(F/A) --> !,
   { functor(Head, F, A), head_worker(Head, Work)},
   [ (:- discontiguous('$cctabled'/2))
   , '$cctabled'(F, A)
   , (Head :- cctabled(Work))
   ].
decl(Moded) -->
   { Moded =.. [F|Args], length(Args, A),
     append(CoreArgs, [ModeSpec], Args),
     must_be(list(var), CoreArgs)
   },
   (  {var(ModeSpec)}
   -> decl(F/A)
   ;  { append(CoreArgs, [LastArg], FullArgs),
        atom_concat(F,'#',W),
        Head =.. [F|FullArgs],
        Work =.. [W|CoreArgs]
      },
      [ (:- discontiguous('$cctabled'/2))
      , '$cctabled'(F, A)
      , (Head :- cctabled(Work, LastArg, ModeSpec))
      ]
   ).

rename_tabled(Extra, Head, Work) :-
   prolog_load_context(module, M),
   current_predicate(M:'$cctabled'/2),
   functor(Head, F, A), A2 is A+Extra,
   M:'$cctabled'(F,A2),
   head_worker(Head, Work).

expand((:- table(Specs)), Clauses) :- !, foldl_clist(decl, Specs, Clauses, []).
expand((Head, P --> Body), (Head2, P --> Body)) :- !, rename_tabled(2, Head, Head2).
expand((Head --> Body),    (Head2 --> Body))    :- !, rename_tabled(2, Head, Head2).
expand((Head :- Body),     (Head2 :- Body))     :- !, rename_tabled(0, Head, Head2).
expand(Head,               Head2)               :- rename_tabled(0, Head, Head2).

system:term_expansion(T1, T2) :- expand(T1, T2).
