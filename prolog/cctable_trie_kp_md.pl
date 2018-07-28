:- module(cctable_trie_kp_md, [run_tabled/1, cctabled/3, lattice/4, po/4, to/4]).
/** <module> Tabling using delimited control

   This version is based on cctable_trie_kp but with answer subsumption
   (ie mode directed tabling).

   Answer subsumption is controlled by an arbitrary ternary predicate that must
   semideterministically combine a new answer with an existing one. Three ready made
   predicates are provided for this purpose: =|to/4|=, =|po/4|= and =|lattice/4|=.
   =|to(Op)|= specifies a total order using the binary predicate Op. Eg, =|to(<)|=
   keeps the minimum.  =|po(Op)|= specifies a partial order, e.g if =|shorter(X,Y)|=
   succeeds when X is a shorter list than Y, then =|po(shorter)|= below keeps the shorter
   of two lists or both if they are the same length. Finally, =|lattice(Join)|= represents
   a lattice: two answer are combined using the given join (+,+,-) operator.
*/

:- use_module(library(ccnbref), [run_nb_ref/1, nbref_new/2]).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(lambdaki)).

%% cctabled(+Work:callable) is det.
:- meta_predicate cctabled(1,-,3).
cctabled(Work, M, J) :- p_shift(tab, md(J,Work,M)).

%% run_tabled(+G:callable) is det.
:- meta_predicate run_tabled(0).
run_tabled(Goal) :-
   term_variables(Goal, Ans), trie_new(Trie),
   run_nb_ref(run_tab(Goal, Trie, Ans)).

run_tab(Goal, Trie, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Trie, Ans).

cont_tab(done, _, _).
cont_tab(susp(md(Join,Work,M), Cont), Trie, Ans) :-
   term_variables(Work,Y), K = k(Y,M,Ans,Cont),
   (  trie_lookup(Trie, Work, tab(Solns,Conts))
   -> lref_add(Conts, K),
      trie_gen(Solns, Y, Ms), member(M, Ms),
      run_tab(Cont, Trie, Ans)
   ;  lref_new(Conts),
      trie_new(Solns),
      trie_insert(Trie, Work, tab(Solns,Conts)),
      (  run_tab(producer(Join, \Y^Work, Conts, Solns, Ans), Trie, Ans)
      ;  trie_gen(Solns, Y, Ms), member(M,Ms), run_tab(Cont, Trie, Ans)
      )
   ).

producer(Join, Generate, Conts, Solns, Ans) :-
   call(Generate, Y, M),
   (  trie_lookup(Solns, Y, Ms0)
   -> call(Join, M, Ms0, Ms1),
      trie_update(Solns, Y, Ms1)
   ;  trie_insert(Solns, Y, [M])
   ),
   lref_get(Conts,Ks), member(k(Y,M,Ans,Cont),Ks), call(Cont).

lref_new(Ref) :- nbref_new([], Ref).
lref_get(Ref, Xs) :- nb_getval(Ref, Ys), copy_term(Ys,Xs).
lref_add(Ref, K) :- duplicate_term(K,K1), nb_getval(Ref, Ks), nb_linkval(Ref, [K1|Ks]).

:- meta_predicate lattice(3,+,+,-), to(2,+,+,-), po(2,+,+,-).
lattice(Join, X, [Y], [Z]) :- call(Join, X, Y, Z), Y \= Z.
to(Op, M, [M0], [M]) :- call(Op, M, M0).  % total order
po(PO, M, Ms, [M|Ms1]) :- % partial order
   maplist(not_worse_than_or_eq(PO,M), Ms),
   exclude(call(PO,M), Ms, Ms1).

not_worse_than_or_eq(PO, X, Y) :- Y \= X, \+call(PO, Y, X).
