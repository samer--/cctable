:- module(cctable_mono_lc, [run_tabled/1, cctabled/1, get_tables/1]).
/** <module> Tabling using multi-prompt delimited control

   This module provides a declarative implementation of tabling using delimited
   continuations to manage the state of the tables and to implement tabling
   itself.

   The entire state is stored in one non-backtrackable global variable,
   which is conceptually simple but terrible for performance because
   the state is copied each time it is set.
*/

:- use_module(library/terms,    [numbervars_copy/2]).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(ccstate), [run_nb_state/3, app/1, get/1]).
:- use_module(library(rbutils)).
:- use_module(library(lambdaki)).


%% cctabled(+Work:callable) is det.
:- meta_predicate cctabled(0).
cctabled(Work) :- p_shift(tab, Work).

%% run_tabled(+Goal:callable) is det.
:- meta_predicate run_tabled(0).
run_tabled(Goal) :-
   rb_empty(Empty),
   term_variables(Goal, Ans),
   run_nb_state(run_tab(Goal, Ans), Empty, _).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(Work, Cont), Ans) :-
   term_variables(Work,Y), K= \Y^Ans^Cont,
   numbervars_copy(Work, VC),
   app(new_cont(VC, K, A)),
   (  A = solns(Ys) -> rb_in(Y, _, Ys), run_tab(Cont, Ans)
   ;  A = new_producer -> run_tab(producer(VC, \Y^Work, K, Ans), Ans)
   ).

new_cont(VC, K, solns(Ys)) --> rb_upd(VC, tab(Ys,Ks), tab(Ys,[K|Ks])).
new_cont(VC, _, new_producer) --> {rb_empty(Ys)}, rb_add(VC, tab(Ys,[])).

producer(VC, Generate, KP, Ans) :-
   call(Generate, Y),
   app(new_soln(VC, Y, Ks)),
   member(K,[KP|Ks]), call(K,Y,Ans).

new_soln(VC, Y, Ks, Tabs1, Tabs2) :-
   rb_upd(VC, tab(Ys1, Ks), tab(Ys2, Ks), Tabs1, Tabs2),
   rb_add(Y, t, Ys1, Ys2).

get_tables(Tables) :- get(Tabs), rb_map(Tabs, sanitise, Tables).
sanitise(tab(S,_), SL) :- rb_keys(S,SL).
