:- module(cctable0, [run_tabled/1, run_tabled/2, cctabled/1, get_tables/1]).
/** <module> Tabling using multi-prompt delimited control

   This module provides a declarative implementation of tabling using delimited
   continuations to manage the state of the tables and to implement tabling
   itself.

   Like cctable0, but taking producer continuation out of producer parameters.
   Also using app/2 instead of get and set to manipulate state.
   Also avoiding lambda copying when calling consumers since state has just been copied.
*/

:- use_module(library/terms,    [numbervars_copy/2]).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(ccstate), [run_nb_state/3, app/1, get/1]).
:- use_module(library(rbutils)).
:- use_module(library(lambdaki)).


%% cctabled(+Head:callable) is det.
%  Call tabled version of Head. Only works in the context of run_tabled/2 or
%  run_tabled/1, which provide the context for state and tabling effects.
:- meta_predicate cctabled(0).
cctabled(Head) :- p_shift(tab, Head).

%% run_tabled(+G:callable, -Tables:rbtree) is det.
%% run_tabled(+G:callable) is det.
%  Run G in a context which supports tabling. Tabled predicates are called
%  using cctabled/1. Predicates can be statically annoted as tabled and calls
%  cctabled/1 introduced using the source transformations in ccmacros.pl.
:- meta_predicate run_tabled(0), run_tabled(0,-).
run_tabled(Goal) :- run_tabled(Goal,_).
run_tabled(Goal, FinalTables) :-
   rb_empty(Tables),
   term_variables(Goal, Ans),
   run_nb_state(run_tab(Goal, Ans), Tables, FinalTables).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(Head, Cont), Ans) :-
   term_variables(Head,Y),
   numbervars_copy(Head, VC),
   app(new_cont(VC, k(Y,Ans,Cont), A)),
   (  A = solns(Solns) -> rb_in(Y, _, Solns), run_tab(Cont, Ans)
   ;  A = new_producer -> run_tab(producer(VC, \Y^Head, Ans), Ans)
   ).

new_cont(VC, K, solns(Solns)) --> rb_upd(VC, tab(Solns,[K0|Ks]), tab(Solns,[K0,K|Ks])).
new_cont(VC, K, new_producer) --> {rb_empty(Solns)}, rb_add(VC, tab(Solns,[K])).

producer(VC, Generate, Ans) :-
   call(Generate, Y),
   app(new_soln(VC, Y, Ks)),
   member(k(Y,Ans,Cont),Ks), call(Cont).

new_soln(VC, Y, Ks, Tabs1, Tabs2) :-
   rb_upd(VC, tab(Solns1, Ks), tab(Solns2, Ks), Tabs1, Tabs2),
   rb_add(Y, t, Solns1, Solns2).

get_tables(Tables) :- get(Tabs), rb_map(Tabs, sanitise, Tables).
sanitise(tab(S,_), SL) :- rb_keys(S,SL).
