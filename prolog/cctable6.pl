:- module(cctable6, [run_tabled/1, cctabled/1, get_tables/1]).
/** <module> Tabling using multi-prompt delimited control

   This module provides a declarative implementation of tabling using delimited
   continuations to manage the state of the tables and to implement tabling
   itself. Similar to cctable0, but using a much faster system for managing
   nonbacktrackable state.

   Using SWI tries
   Using mutable list references for continuations
   Not representing VCs as ground terms (not necessary for trie)
*/

:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(lambdaki)).

%% cctabled(+Head:callable) is det.
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
   b_setval('tab.trie', Trie),
   setup_call_cleanup(lref_init(P), run_tab(Goal, Trie, P, Ans), 
                      lref_cleanup(P)).

run_tab(Goal, Trie, Prefix, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Trie, Prefix, Ans).

cont_tab(done, _, _, _).
cont_tab(susp(Head, Cont), Trie, Prefix, Ans) :-
   term_variables(Head,Y), K = k(Y,Ans,Cont),
   (  trie_lookup(Trie, Head, tab(Solns,Conts))
   -> lref_prepend(Conts, K),
      trie_gen(Solns, Y, _), run_tab(Cont, Trie, Prefix, Ans)
   ;  lref_new(Prefix, Conts),
      trie_new(Solns),
      trie_insert(Trie, Head, tab(Solns,Conts)),
      run_tab(producer(Conts, Solns, \Y^Head, K, Ans), Trie, Prefix, Ans)
   ).

producer(Conts, Solns, Generate, KP, Ans) :-
   call(Generate, Y), trie_insert(Solns, Y, t),
   lref_get(Conts,Ks), member(k(Y,Ans,Cont),[KP|Ks]), call(Cont).

get_tables(TablesTree) :-
   b_getval('tab.trie', Trie),
   findall(Head-SL, trie_variant_class_solutions(Trie, Head, SL), Tables),
   list_to_rbtree(Tables, TablesTree).

trie_variant_class_solutions(Trie, Head, Solns) :-
   trie_gen(Trie, Head, tab(SolnsTrie, _)),
   numbervars(Head, 0, _), 
   findall(S, trie_gen(SolnsTrie,S,_), Solns).

% ---  references to growable lists ----
lref_init(Prefix) :-
   gensym(lref,ID), atom_concat(ID,'.',Prefix),
   nb_setval(Prefix, 0).

lref_cleanup(Prefix) :-
   nb_getval(Prefix, N), nb_delete(Prefix),
   forall(between(1,N,I), (atomic_concat(Prefix,I,Ref), nb_delete(Ref))).

lref_get(Ref, Xs) :- nb_getval(Ref, Ys), copy_term(Ys,Xs).
lref_prepend(Ref, X) :- duplicate_term(X,X1), nb_getval(Ref, Xs), nb_linkval(Ref, [X1|Xs]).
lref_new(Prefix, Ref) :-
   nb_getval(Prefix, I), J is I+1, atomic_concat(Prefix, J, Ref),
   nb_setval(Ref, []), nb_setval(Prefix,J).
