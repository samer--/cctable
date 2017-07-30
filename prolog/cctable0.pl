:- module(cctable0, [run_tabled/1, run_tabled/2, cctabled/1]).
/** <module> Tabling using multi-prompt delimited control

   This module provides a declarative implementation of tabling using delimited
   continuations to manage the state of the tables and to implement tabling
   itself.

   The entire state is stored in one non-backtrackable global variable,
   which is conceptually simple but terrible for performance because
   the state is copied each time it is set.
*/

:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(ccstate), [run_nb_state/3, set/1, get/1]).
:- use_module(library(rbutils)).
:- use_module(lambdaki).


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

head_to_variant(Head, Variant) :-
   copy_term_nat(Head, Variant),
   numbervars(Variant, 0, _).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(Head, Cont), Ans) :-
   term_variables(Head,Y), K= \Y^Ans^Cont,
   get(Tabs1),
   head_to_variant(Head, Variant),
   (  rb_update(Tabs1, Variant, tab(Solns,Ks), tab(Solns,[K|Ks]), Tabs2)
   -> set(Tabs2),
      rb_in(Y, _, Solns),
      run_tab(Cont, Ans)
   ;  rb_empty(Solns),
      rb_insert_new(Tabs1, Variant, tab(Solns,[]), Tabs2),
      set(Tabs2),
      run_tab(producer(Variant, \Y^Head, K, Ans), Ans)
   ).

producer(Variant, Generate, KP, Ans) :-
   call(Generate, Y1),
   get(Tabs1),
   rb_update(Tabs1, Variant, tab(Solns1, Ks), tab(Solns2, Ks), Tabs2),
   rb_insert_new(Solns1, Y1, t, Solns2),
   set(Tabs2),
   member(K,[KP|Ks]),
   call(K,Y1,Ans).
