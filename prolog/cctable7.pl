:- module(cctable7, [run_tabled/1, cctabled/1, get_tables/1]).
/** <module> Tabling using multi-prompt delimited control

   Like cctable6, but using nb_sets for solutions, partly to
   check correctness of tries. There was a bug in the tries,
   but this is fixed now.
*/

:- use_module(library/nbref, [with_nbref/2, nbref_new/3]).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(lambdaki)).


%% cctabled(+Work:callable) is det.
:- meta_predicate cctabled(0).
cctabled(Work) :- p_shift(tab, Work).

%% run_tabled(+G:callable) is det.
%  Run G in a context which supports tabling. Tabled predicates are called
%  using cctabled/1. Predicates can be statically annoted as tabled and calls
%  cctabled/1 introduced using the source %  transformations in ccmacros.pl.
:- meta_predicate run_tabled(0).
run_tabled(Goal) :-
   term_variables(Goal, Ans),
   with_trie(Trie, (b_setval('tab.trie', Trie), % ugly hack, only for get_tables/1
                    with_nbref(NBR, run_tab(Goal,env(Trie,NBR),Ans)))).

run_tab(Goal, Env, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Env, Ans).

cont_tab(done, _, _).
cont_tab(susp(Work, Cont), Env, Ans) :-
   Env=env(Trie,NBR),
   term_variables(Work,Y), K = k(Y,Ans,Cont),
   (  trie_lookup(Trie, Work, tab(Solns,Conts))
   -> lref_prepend(Conts, K),
      sref_gen(Solns, Y), run_tab(Cont, Env, Ans)
   ;  lref_new(NBR, Conts),
      sref_new(NBR, Solns),
      trie_insert(Trie, Work, tab(Solns,Conts)),
      run_tab(producer(Conts, Solns, \Y^Work, K, Ans), Env, Ans)
   ).

producer(Conts, Solns, Generate, KP, Ans) :-
   call(Generate, Y), 
   sref_add(Solns, Y),
   lref_get(Conts,Ks), 
   member(k(Y,Ans,Cont),[KP|Ks]), call(Cont).

get_tables(TablesTree) :-
   b_getval('tab.trie', Trie),
   findall(Work-SL, trie_variant_class_solutions(Trie, Work, SL), Tables),
   list_to_rbtree(Tables, TablesTree).

trie_variant_class_solutions(Trie, Work, SolnsList) :-
   trie_gen(Trie, Work, tab(Solns, _)),
   numbervars(Work, 0, _), 
   findall(S, sref_gen(Solns,S), SolnsList).

% ---  references to growable lists ----
with_trie(Trie, Goal) :- setup_call_cleanup(trie_new(Trie), Goal, trie_destroy(Trie)).

lref_get(Ref, Xs) :- nb_getval(Ref, Ys), copy_term(Ys,Xs).
lref_prepend(Ref, X) :- duplicate_term(X,X1), nb_getval(Ref, Xs), nb_linkval(Ref, [X1|Xs]).
lref_new(Prefix, Ref) :- nbref_new(Prefix, [], Ref).

sref_gen(Ref, X) :- nb_getval(Ref, Ys), gen_nb_set(Ys, X).
sref_add(Ref, X) :- duplicate_term(X,X1), nb_getval(Ref, Xs), add_nb_set(X1,Xs,true).
sref_new(Prefix, Ref) :- empty_nb_set(Empty), nbref_new(Prefix, Empty, Ref).
