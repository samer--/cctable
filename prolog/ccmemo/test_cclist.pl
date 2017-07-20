#!/usr/bin/env swipl
:- module(test_cclist, []).

:- use_module(library(math)).
:- use_module(library(dcg_core), [rep//2]).
:- use_module(library(delimcc), [ccshell/0]).
:- use_module(library(ccstate),  [run_ref/1]).
:- use_module(cclist, [guard/1, choose/2, run_list/2, memo_nondet/2, memo_nondet/3]).

% ------- DCG utilities ----------

% binary choice between two DCG goals
:- op(1100, xfy, <+>).
(P <+> Q) --> 
   {choose([0,1],I)},
   ({I=0} -> call_dcg(P); call_dcg(Q)).

% match terminal (for use in grammars)
t(_,[],_) :- !, guard(false).
t(W,[X|T],T) :- guard(W=X).

% -------- test programs ---------

test1(r(X)) :- 
   choose([1,2,3],X).

test2(Y) :-
   choose([1,2,3],X),
   choose([a(X),b(X),c(X)],Y).

link(a,X) :- choose([b,c],X).
link(b,d).
link(c,X) :- choose([d,h],X).
link(d,X) :- choose([e,f],X).
link(e,g).
link(f,X) :- choose([g,h],X).
link(g,h).
link(h,X) :- choose([],X).

% open recursive path predicates (in DCG form)
pathr(P) --> link, ([] <+> P).
pathl(P) --> ([] <+> P), link.

mem_path(left, P) :- memo_nondet(pathl(P), P).
mem_path(right, P) :- memo_nondet(pathr(P), P).
test_path(LR, Start, Ends) :-
   mem_path(LR, P), run_list(call(P,Start), Ends).

% left and right recursive grammars from Frost et al.
s(S)      --> [] <+> t(a), S, S.
sl(S)     --> [] <+> S, S, t(a).
sl(S,Aux) --> [] <+> S, Aux.
aux(S)    --> S, t(a).

grammar(s,S) :- S=s(S).
grammar(m(sm),S) :- memo_nondet(s(S),S).
grammar(m(sml),S) :- memo_nondet(sl(S),S).
grammar(m(smml),S) :- memo_nondet(sl(S,Aux),S), memo_nondet(aux(S),Aux).

test_grammar(G,N) :-
   between(0,96,N),
   rep(N,[a],L,[]),
   grammar(G,S),
   time(run_list(call(S,L), _)).

:- module(test_cclist).
% vim: ft=prolog
