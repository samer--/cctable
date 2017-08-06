:- module(cctable7, [run_tabled/1, cctabled/1, get_tables/1]).
/** <module> Tabling using multi-prompt delimited control

   This module provides a declarative implementation of tabling using delimited
   continuations to manage the state of the tables and to implement tabling
   itself. Similar to cctable0, but using a much faster system for managing
   nonbacktrackable state.

   Like cctable6, but using nb_sets for solutions
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
   term_variables(Goal, Ans),
   setup_call_cleanup(setup(Env), run_tab(Goal,Env,Ans), cleanup(Env)).

setup(env(Trie,NBR)) :- trie_new(Trie), nbref_init(NBR), b_setval('tab.trie', Trie).
cleanup(env(Trie,NBR)) :- trie_destroy(Trie), nbref_cleanup(NBR).

run_tab(Goal, Env, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Env, Ans).

cont_tab(done, _, _).
cont_tab(susp(Head, Cont), Env, Ans) :-
   Env=env(Trie,NBR),
   term_variables(Head,Y), K = k(Y,Ans,Cont),
   (  trie_lookup(Trie, Head, tab(Solns,Conts))
   -> lref_prepend(Conts, K),
      sref_gen(Solns, Y), run_tab(Cont, Env, Ans)
   ;  lref_new(NBR, Conts),
      sref_new(NBR, Solns),
      trie_insert(Trie, Head, tab(Solns,Conts)),
      run_tab(producer(Conts, Solns, \Y^Head, K, Ans), Env, Ans)
   ).

producer(Conts, Solns, Generate, KP, Ans) :-
   call(Generate, Y), 
   sref_add(Solns, Y),
   lref_get(Conts,Ks), 
   member(k(Y,Ans,Cont),[KP|Ks]), call(Cont).

get_tables(TablesTree) :-
   b_getval('tab.trie', Trie),
   findall(Head-SL, trie_variant_class_solutions(Trie, Head, SL), Tables),
   list_to_rbtree(Tables, TablesTree).

trie_variant_class_solutions(Trie, Head, SolnsList) :-
   trie_gen(Trie, Head, tab(Solns, _)),
   numbervars(Head, 0, _), 
   findall(S, sref_gen(Solns,S), SolnsList).

% ---  references to growable lists ----
nbref_init(Prefix) :-
   gensym(nbref,ID), atom_concat(ID,'.',Prefix),
   nb_setval(Prefix, 0).

nbref_cleanup(Prefix) :-
   nb_getval(Prefix, N), nb_delete(Prefix),
   forall(between(1,N,I), (atomic_concat(Prefix,I,Ref), nb_delete(Ref))).

nbref_new(Prefix, Value, Ref) :-
   nb_getval(Prefix, I), J is I+1, atomic_concat(Prefix, J, Ref),
   nb_setval(Ref, Value), nb_setval(Prefix,J).

lref_get(Ref, Xs) :- nb_getval(Ref, Ys), copy_term(Ys,Xs).
lref_prepend(Ref, X) :- duplicate_term(X,X1), nb_getval(Ref, Xs), nb_linkval(Ref, [X1|Xs]).
lref_new(Prefix, Ref) :- nbref_new(Prefix, [], Ref).
sref_gen(Ref, X) :- nb_getval(Ref, Ys), gen_nb_set(Ys, X).
sref_add(Ref, X) :- duplicate_term(X,X1), nb_getval(Ref, Xs), add_nb_set(X1,Xs,true).
sref_new(Prefix, Ref) :- empty_nb_set(Empty), nbref_new(Prefix, Empty, Ref).
