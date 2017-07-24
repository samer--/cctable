:- module(cctable2, [run_tabled/1, cctabled/1]).
/** <module> Tabling using multi-prompt delimited control

   This module provides a declarative implementation of tabling using delimited
   continuations to manage the state of the tables and to implement tabling
   itself. Similar to cctable, but using a much faster system for managing
   nonbacktrackable state.
*/

:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(ccstate), [run_nb_ref/1, nbr_app/2, nbr_app_or_new/3]).
:- use_module(library(rbutils)).
:- use_module(lambdaki).


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
   run_nb_ref(run_tab(Goal, Ans)).

head_to_variant(Head, Variant) :-
   copy_term_nat(Head, Variant),
   numbervars(Variant, 0, _).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(Head, Cont), Ans) :-
   term_variables(Head,Y), K = \Y^Ans^Cont,
   head_to_variant(Head, Variant),
   nbr_app_or_new(Variant, new_consumer(Res,K), new_producer(Res)),
   (  Res = solns(Solns) -> rb_gen(Y, _, Solns), run_tab(Cont, Ans)
   ;  Res = new_producer -> run_tab(producer(Variant, \Y^Head, K, Ans), Ans)
   ).

new_consumer(solns(Solns), K, tab(Solns,Ks), tab(Solns,[K|Ks])).
new_producer(new_producer, tab(Solns,[])) :- rb_empty(Solns).

producer(Variant, Generate, KP, Ans) :-
   call(Generate, Y1),
   nbr_app(Variant, new_soln(Y1,Res)),
   Res = new(Ks), member(K,[KP|Ks]), call(K,Y1,Ans).

new_soln(Y1, Res, tab(Solns1,Ks), tab(Solns2,Ks)) :-
   rb_upd_or_ins(Y1, Action, Solns1, Solns2),
   ( Action-Res = insert(t)-new(Ks)
   ; Action-Res = update(t,t)-old
   ).
