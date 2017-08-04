:- module(cctable5, [run_tabled/1, cctabled/1, get_tables/1]).
/** <module> Tabling using multi-prompt delimited control

   This module provides a declarative implementation of tabling using delimited
   continuations to manage the state of the tables and to implement tabling
   itself. Similar to cctable0, but using a much faster system for managing
   nonbacktrackable state.

   Avoiding lambda copying.
   Using SWI tries
*/

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
   trie_new(Trie),
   term_variables(Goal, Ans),
   setup_call_cleanup(nb_setval(tabling_trie, Trie),
                      run_tab(Goal, Ans),
                      nb_delete(tabling_trie)).

head_to_variant_class(Head, VC) :-
   copy_term_nat(Head, VC),
   numbervars(VC, 0, _).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(Head, Cont), Ans) :-
   nb_getval(tabling_trie, Trie),
   term_variables(Head,Y), K = k(Y,Ans,Cont),
   head_to_variant_class(Head, VC),
   (  trie_lookup(Trie, VC, tab(Solns,Ks))
   -> trie_update(Trie, VC, tab(Solns,[K|Ks])), % !! potentially expensive copy here
      trie_gen(Solns, Y, _), run_tab(Cont, Ans) % should we copy Y or stick to grounds?
   ;  trie_new(Solns),
      trie_insert(Trie, VC, tab(Solns,[])),
      run_tab(producer(Trie, VC, Solns, \Y^Head, K, Ans), Ans)
   ).

new_producer(new_producer, tab(Solns,[])) :- rb_empty(Solns).

producer(Trie, VC, Solns, Generate, KP, Ans) :-
   call(Generate, Y),
   trie_insert(Solns, Y, t),
   trie_lookup(Trie, VC, tab(_,Ks)), % nb, value is copied on lookup
   member(k(Y,Ans,Cont),[KP|Ks]), call(Cont).

new_soln(Y, Ks, tab(Ys1,Ks), tab(Ys2,Ks)) :- rb_add(Y,t,Ys1,Ys2).

get_tables(TablesTree) :-
   nb_getval(tabling_trie,Trie),
   findall(VC-SL, trie_variant_class_solutions(Trie, VC, SL), Tables),
   list_to_rbtree(Tables, TablesTree).

trie_variant_class_solutions(Trie, VC, Solns) :-
   trie_gen(Trie, VC, tab(SolnsTrie, _)),
   findall(S, trie_gen(SolnsTrie,S,_), Solns).
