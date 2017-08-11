:- module(cctable_trie_1p, [run_tabled/1, cctabled/1, get_tables/1]).
/** <module> Tabling using multi-prompt delimited control

   This version is based on cctable5 (using tries) but now
   the list of consumer continations for each variant class is
   represented as an immutable reference to a mutable list
   which cheap to grow, avoiding a quadratic update cost.
   Single prompt version.
*/

:- use_module(library/nbref, [with_nbref/2, nbref_new/3]).
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
                      with_nbref(NBR, run_tab(Goal, Trie, NBR, Ans)),
                      nb_delete('tab.trie')).

run_tab(Goal, Trie, NBR, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Trie, NBR, Ans).

cont_tab(done, _, _, _).
cont_tab(susp(Work, Cont), Trie, NBR, Ans) :-
   term_variables(Work,Y), K = k(Y,Ans,Cont),
   (  trie_lookup(Trie, Work, tab(Solns,Conts))
   -> lref_prepend(Conts, K),
      trie_gen(Solns, Y, _), run_tab(Cont, Trie, NBR, Ans)
   ;  lref_new(NBR, Conts),
      trie_new(Solns),
      trie_insert(Trie, Work, tab(Solns,Conts)),
      run_tab(producer(\Y^Work, K, Conts, Solns, Ans), Trie, NBR, Ans)
   ).

producer(Generate, KP, Conts, Solns, Ans) :-
   call(Generate, Y), trie_insert(Solns, Y, t),
   lref_get(Conts,Ks), member(k(Y,Ans,Cont),[KP|Ks]), call(Cont).

get_tables(TablesTree) :-
   b_getval('tab.trie', Trie),
   findall(Work-SL, trie_variant_class_solutions(Trie, Work, SL), Tables),
   list_to_rbtree(Tables, TablesTree).

trie_variant_class_solutions(Trie, Work, Solns) :-
   trie_gen(Trie, Work, tab(SolnsTrie, _)),
   numbervars(Work, 0, _),
   findall(S, trie_gen(SolnsTrie,S,_), Solns).

% ---  references to growable lists ----
lref_new(NBR, Ref) :- nbref_new(NBR, [], Ref).
lref_get(Ref, Xs) :- nb_getval(Ref, Ys), copy_term(Ys,Xs).
lref_prepend(Ref, X) :- duplicate_term(X,X1), nb_getval(Ref, Xs), nb_linkval(Ref, [X1|Xs]).
