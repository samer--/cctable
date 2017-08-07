:- module(cctable5, [run_tabled/1, cctabled/1, get_tables/1]).
/** <module> Tabling using multi-prompt delimited control

   This version uses SWI Prolog's tries library to manage the map
   for variant classes to tables, and also to represent the set of
   solutions for each variant class. Also, there is no longer any
   need to represent variant classes as ground terms as the trie
   library works on variant terms directly.
*/

:- use_module(library/terms,    [numbervars_copy/2]).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
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
   b_setval('tab.trie', Trie),
   run_tab(Goal, Trie, Ans).

run_tab(Goal, Trie, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Trie, Ans).

cont_tab(done, _, _).
cont_tab(susp(Head, Cont), Trie, Ans) :-
   term_variables(Head,Y), K = k(Y,Ans,Cont),
   numbervars_copy(Head, VC),
   (  trie_lookup(Trie, VC, tab(Solns,Ks))
   -> trie_update(Trie, VC, tab(Solns,[K|Ks])), % !! potentially expensive copy here
      trie_gen(Solns, Y, _), run_tab(Cont, Trie, Ans) % should we copy Y or stick to grounds?
   ;  run_tab(producer(VC, \Y^Head, K, Trie, Ans), Trie, Ans)
   ).

producer(VC, Generate, KP, Trie, Ans) :-
   trie_new(Solns),
   trie_insert(Trie, VC, tab(Solns,[])),
   call(Generate, Y),
   trie_insert(Solns, Y, t),
   trie_lookup(Trie, VC, tab(_,Ks)), % nb, value is copied on lookup
   member(k(Y,Ans,Cont),[KP|Ks]), call(Cont).

get_tables(TablesTree) :-
   b_getval('tab.trie', Trie),
   findall(VC-SL, trie_variant_class_solutions(Trie, VC, SL), Tables),
   list_to_rbtree(Tables, TablesTree).

trie_variant_class_solutions(Trie, VC, Solns) :-
   trie_gen(Trie, VC, tab(SolnsTrie, _)),
   findall(S, trie_gen(SolnsTrie,S,_), Solns).
