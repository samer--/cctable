:- module(cctable8, [run_tabled/1, cctabled/1, get_tables/1]).
/** <module> Tabling using delimited control

   This version is based on cctable6, but storing the producer
   continuation in the mutable continuations list along with the
   consumers. This reduces the size of the captured continuations.
*/

:- use_module(library/nbref, [with_nbref/2, nbref_new/3]).
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
   nb_setval(cont_sizes,[]),
   term_variables(Goal, Ans),
   with_trie(Trie, (b_setval('tab.trie', Trie), % ugly hack, only for get_tables/1
                    with_nbref(NBR, run_tab(Goal, Trie, NBR, Ans)))).

run_tab(Goal, Trie, NBR, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Trie, NBR, Ans).

cont_tab(done, _, _, _).
cont_tab(susp(Head, Cont), Trie, NBR, Ans) :-
   term_variables(Head,Y), K = k(Y,Ans,Cont),
   (  trie_lookup(Trie, Head, tab(Solns,Conts))
   -> lref_add(Conts, K),
      trie_gen(Solns, Y, _),
      run_tab(Cont, Trie, NBR, Ans)
   ;  lref_new(NBR, K, Conts),
      trie_new(Solns),
      trie_insert(Trie, Head, tab(Solns,Conts)),
      run_tab(producer(\Y^Head, Conts, Solns, Ans), Trie, NBR, Ans)
   ).

producer(Generate, Conts, Solns, Ans) :-
   call(Generate, Y), trie_insert(Solns, Y, t),
   lref_get(Conts,Ks), member(k(Y,Ans,Cont),Ks), call(Cont).

% NB. K0 (producer continuation) is kept at the head of the list.
lref_new(NBR, K0, Ref) :- nbref_new(NBR, [K0], Ref).
lref_get(Ref, Xs) :- nb_getval(Ref, Ys), copy_term(Ys,Xs).
lref_add(Ref, K) :- duplicate_term(K,K1), nb_getval(Ref, [K0|Ks]), nb_linkval(Ref, [K0,K1|Ks]).

with_trie(Trie, Goal) :- setup_call_cleanup(trie_new(Trie), Goal, trie_destroy(Trie)).

get_tables(TablesTree) :-
   b_getval('tab.trie', Trie),
   findall(Head-SL, trie_variant_class_solutions(Trie, Head, SL), Tables),
   list_to_rbtree(Tables, TablesTree).

trie_variant_class_solutions(Trie, Head, Solns) :-
   trie_gen(Trie, Head, tab(SolnsTrie, _)),
   numbervars(Head, 0, _),
   findall(S, trie_gen(SolnsTrie,S,_), Solns).
