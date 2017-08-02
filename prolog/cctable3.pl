:- module(cctable2, [run_tabled/1, cctabled/1, get_tables/1]).
/** <module> Tabling using multi-prompt delimited control

   This module provides a declarative implementation of tabling using delimited
   continuations to manage the state of the tables and to implement tabling
   itself. Similar to cctable0, but using a much faster system for managing
   nonbacktrackable state.

   Combines cctable2 (avoiding lambda copying) and cctable1 (split solns from conts)
   However, by avoiding copying the list of continuations in producer/4, we may
   end up having to copy each continuation in order to call it safely?
*/

:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(ccnbenv), [run_nb_env/1, nb_get/2, nb_app/2, nb_new/2, nb_get_or_new/3, nb_dump/1]).
:- use_module(library(rbutils)).
:- use_module(library(lambdaki)).


%% cctabled(+Head:callable) is det.
%  Call tabled version of Head. Only works in the context of run_tabled/2 or
%  run_tabled/1, which provide the context for state and tabling effects.
:- meta_predicate cctabled(0).
cctabled(Head) :- p_shift(tab, Head).

%% run_tabled(+G:callable) is det.
%  Run G in a context which supports tabling. Tabled predicates are called
%  using cctabled/1. Predicates can be statically annoted as tabled and calls
%  cctabled/1 introduced using the source %  transformations in ccmacros.pl.
:- meta_predicate run_tabled(0).
run_tabled(Goal) :-
   term_variables(Goal, Ans),
   copy_term(Ans-Goal, Ans1-Goal1),
   run_nb_env(run_tab(call_unify(Goal1,Ans1,Ans), Ans)).

head_to_variant_class(Head, VC) :-
   copy_term_nat(Head, VC),
   numbervars(VC, 0, _).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(Head, Cont), Ans) :-
   term_variables(Head,Y), K = \Y^Ans^Cont,
   head_to_variant_class(Head, VC),
   nb_get_or_new(solns(VC), old_table(Res), new_table(Res)),
   (  Res = solns(Solns) -> nb_app(conts(VC), cons(K)), rb_in(Y, _, Solns), run_tab(Cont, Ans)
   ;  Res = new_table    -> nb_new(conts(VC), =([])), run_tab(producer(VC, Y-Head, K, Ans), Ans)
   ).

old_table(solns(Solns), Solns).
new_table(new_table, Solns) :- rb_empty(Solns).
cons(K,Ks, [K|Ks]).

producer(VC, Generate, KP, Ans) :-
   call_snd_fst(Generate, Y),
   nb_app(solns(VC), rb_add(Y,t)),
   nb_get(conts(VC), Ks),
   member(K,[KP|Ks]), call(K,Y,Ans).

get_tables(Tables) :- nb_dump(Raw), rb_empty(E), once(rb_fold(sanitise, Raw, E, Tables)).
sanitise(conts(_)-_,T,T).
sanitise(solns(V)-S,T1,T2) :- rb_keys(S,SL), rb_add(V,SL,T1,T2).

call_unify(G,A1,A2) :- call(G), A1=A2.
call_snd_fst(X-G, X) :- call(G).
