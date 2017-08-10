:- module(cctable_db_1p, [run_tabled/1, cctabled/1, get_tables/1]).
/** <module> Tabling using delimited control

   This module provides a declarative implementation of tabling using delimited control.
   This version uses thread local dynamic predicates to manage the table state.
   Also only 1 prompt and no lambda copying continuations.
*/

:- use_module(library/terms,    [numbervars_copy/2]).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(rbutils)).
:- use_module(library(lambdaki)).

:- thread_local producer/1, consumer/2, solution/2.

%% cctabled(+Work:callable) is det.
:- meta_predicate cctabled(0).
cctabled(Work) :- p_shift(tab, Work).

%% run_tabled(+G:callable) is det.
:- meta_predicate run_tabled(0).
run_tabled(Goal) :-
   term_variables(Goal, Ans),
   call_cleanup(run_tab(Goal, Ans), cleanup).

cleanup :-
   retractall(producer(_)),
   retractall(solution(_,_)),
   retractall(consumer(_,_)).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(Work, Cont), Ans) :-
   term_variables(Work,Y), K = k(Y,Ans,Cont),
   numbervars_copy(Work, VC),
   (  producer(VC)
   -> assert(consumer(VC, K)), solution(VC,Y), run_tab(Cont, Ans)
   ;  assert(producer(VC)), run_tab(producer(VC, \Y^Work, K, Ans), Ans)
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
gather(VC,Solns) :- producer(VC), findall(S,solution(VC,S),Solns).
