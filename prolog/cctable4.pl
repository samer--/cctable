:- module(cctable4, [run_tabled/1, cctabled/1, get_tables/1]).
/** <module> Tabling using multi-prompt delimited control

   This module provides a declarative implementation of tabling using delimited
   continuations to manage the state of the tables and to implement tabling
   itself. Similar to cctable0, but using a much faster system for managing
   nonbacktrackable state.

   Using dynamic DB instead of nb state
*/

:- use_module(library/terms,    [numbervars_copy/2]).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(rbutils)).
:- use_module(library(lambdaki)).

:- thread_local active/1, consumer/2, solution/2.

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
   call_cleanup(run_tab(Goal, Ans), cleanup).

cleanup :-
   retractall(active(_)),
   retractall(solution(_,_)),
   retractall(consumer(_,_)).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(Head, Cont), Ans) :-
   term_variables(Head,Y), K = k(Y,Ans,Cont),
   numbervars_copy(Head, VC),
   (  active(VC)
   -> assert(consumer(VC, K)), solution(VC,Y), run_tab(Cont, Ans)
   ;  assert(active(VC)), run_tab(producer(VC, \Y^Head, K, Ans), Ans)
   ).

producer(VC, Generate, KP, Ans) :-
   call(Generate, Y),
   \+solution(VC, Y), % ground only!
   assert(solution(VC, Y)),
   (KP=k(Y,Ans,Cont); consumer(VC, k(Y,Ans,Cont))),
   call(Cont).

get_tables(Tables) :-
   findall(VC-Solns, gather(VC,Solns), TablesList),
   list_to_rbtree(TablesList,Tables).
gather(VC,Solns) :- active(VC), findall(S,solution(VC,S),Solns).
