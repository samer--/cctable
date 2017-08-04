:- module(cctable3, [run_tabled/1, cctabled/1, get_tables/1]).
/** <module> Tabling using multi-prompt delimited control

   This module provides a declarative implementation of tabling using delimited
   continuations to manage the state of the tables and to implement tabling
   itself. Similar to cctable0, but using a much faster system for managing
   nonbacktrackable state.

   Avoiding lambda copying.
   Relies on copying in nb_setval
*/

:- use_module(library/nbenv,    [run_nb_env/1, nb_app/2, nb_app_or_new/3, nb_dump/1]).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
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
   run_nb_env(run_tab(Goal, Ans)).

head_to_variant_class(Head, VC) :-
   copy_term_nat(Head, VC),
   numbervars(VC, 0, _).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(Head, Cont), Ans) :-
   term_variables(Head,Y), K = k(Y,Ans,Cont),
   head_to_variant_class(Head, VC),
   nb_app_or_new(VC, new_consumer(Res,K), new_producer(Res)),
   (  Res = solns(Solns) -> rb_in(Y, _, Solns), run_tab(Cont, Ans)
   ;  Res = new_producer -> run_tab(producer(VC, \Y^Head, K, Ans), Ans)
   ).

new_consumer(solns(Solns), K, tab(Solns,Ks), tab(Solns,[K|Ks])).
new_producer(new_producer, tab(Solns,[])) :- rb_empty(Solns).

producer(VC, Generate, KP, Ans) :-
   call(Generate, Y),
   nb_app(VC, new_soln(Y,Ks)),
   member(k(Y,Ans,Cont),[KP|Ks]), call(Cont).

new_soln(Y, Ks, tab(Ys1,Ks), tab(Ys2,Ks)) :- rb_add(Y,t,Ys1,Ys2).

get_tables(Tables) :- nb_dump(Raw), rb_map(Raw, sanitise, Tables).
sanitise(tab(S,_), SL) :- rb_keys(S,SL).
