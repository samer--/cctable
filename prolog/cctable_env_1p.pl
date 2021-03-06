:- module(cctable_env_1p, [run_tabled/1, cctabled/1, get_tables/1]).
/** <module> Tabling using delimited control

   This module provides a declarative implementation of tabling using delimited continuations.
   Based on cctable2, but using direct, not delimited control implementation of nbenv.
*/

:- use_module(library/terms,    [numbervars_copy/2]).
:- use_module(library/nbenv,    [run_nb_env/1, nb_app/2, nb_app_or_new/3, nb_dump/1]).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(rbutils)).
:- use_module(library(lambdaki)).


%% cctabled(+Work:callable) is det.
:- meta_predicate cctabled(0).
cctabled(Work) :- p_shift(tab, Work).

%% run_tabled(+G:callable) is det.
:- meta_predicate run_tabled(0).
run_tabled(Goal) :-
   term_variables(Goal, Ans),
   run_nb_env(run_tab(Goal, Ans)).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(Work, Cont), Ans) :-
   term_variables(Work,Y), K = k(Y,Ans,Cont),
   numbervars_copy(Work, VC),
   nb_app_or_new(VC, new_consumer(Res,K), new_producer(Res)),
   (  Res = solns(Solns) -> rb_in(Y, _, Solns), run_tab(Cont, Ans)
   ;  Res = new_producer -> run_tab(producer(VC, \Y^Work, K, Ans), Ans)
   ).

new_consumer(solns(Solns), K, tab(Solns,Ks), tab(Solns,[K|Ks])).
new_producer(new_producer, tab(Solns,[])) :- rb_empty(Solns).

producer(VC, Generate, KP, Ans) :-
   call(Generate, Y),
   nb_app(VC, new_soln(Y,Ks)),
   member(k(Y,Ans,Cont),[KP|Ks]), call(Cont).

new_soln(Y, Ks, tab(Ys1,Ks), tab(Ys2,Ks)) :- rb_add(Y,t,Ys1,Ys2).

get_tables(Tables) :- nb_dump(Raw), rb_map(Raw, sanitise, Tables).
sanitise(tab(S,_), SL) :- rb_keys(S,SL).
