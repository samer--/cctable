:- module(cctable_trie_kp, [run_tabled/1, cctabled/1, get_tables/1]).
/** <module> Tabling using delimited control

   This version is based on cctable_trie, but storing the producer
   continuation in the mutable continuations list along with the
   consumers. This reduces the size of the captured continuations.
   It also cleans out some inactive frames from the continuations.
   Double prompt version using run_nb_ref/2.
*/

:- use_module(library/ccnbref,  [run_nb_ref/1, nbref_new/2]).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(lambdaki)).

%% cctabled(+Work:callable) is det.
:- meta_predicate cctabled(0).
cctabled(Work) :- p_shift(tab, Work).

%% run_tabled(+G:callable) is det.
:- meta_predicate run_tabled(0).
run_tabled(Goal) :-
   term_variables(Goal, Ans), trie_new(Trie),
   setup_call_cleanup(nb_setval('tab.trie', Trie), % ugly hack, only for get_tables/1
                      run_nb_ref(run_tab(Goal, Trie, Ans)),
                      nb_delete('tab.trie')).

run_tab(Goal, Trie, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Trie, Ans).

cont_tab(done, _, _).
cont_tab(susp(Work, Cont), Trie, Ans) :-
   term_variables(Work,Y), K = k(Y,Ans,Cont),
   (  trie_lookup(Trie, Work, tab(Solns,Conts))
   -> lref_add(Conts, K),
      trie_gen(Solns, Y, _),
      run_tab(Cont, Trie, Ans)
   ;  lref_new(K, Conts),
      trie_new(Solns),
      trie_insert(Trie, Work, tab(Solns,Conts)),
      run_tab(producer(\Y^Work, Conts, Solns, Ans), Trie, Ans)
   ).


producer(Generate, Conts, Solns, Ans) :-
   call(Generate, Y), trie_insert(Solns, Y, t),
   lref_get(Conts,Ks), member(k(Y,Ans,Cont),Ks), call(Cont).

% NB. K0 (producer continuation) is kept at the head of the list.
lref_new(K0, Ref) :- nbref_new([K0], Ref).
lref_get(Ref, Xs) :- nb_getval(Ref, Ys), copy_term(Ys,Xs).
lref_add(Ref, K) :- duplicate_term(K,K1), nb_getval(Ref, [K0|Ks]), nb_linkval(Ref, [K0,K1|Ks]).

get_tables(TablesTree) :-
   nb_getval('tab.trie', Trie),
   findall(Work-SL, trie_variant_class_solutions(Trie, Work, SL), Tables),
   list_to_rbtree(Tables, TablesTree).

trie_variant_class_solutions(Trie, Work, Solns) :-
   trie_gen(Trie, Work, tab(SolnsTrie, _)),
   numbervars(Work, 0, _),
   findall(S, trie_gen(SolnsTrie,S,_), Solns).
