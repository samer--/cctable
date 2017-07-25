:- module(ccmemo, [run_list/2, choose/2, guard/1, memo_nondet/3, memo_nondet/2]).

/** <module> Nondeterminism as a list with recursive memoisation

This module uses delimited continuations to provided nondeterminism as a
control effect along with memoisation of recursive, nondeterministic
binary relations. The effect is reified as a list.

Use run_list/2 to run a unary predicate in a context that supports
choose/2 for nondetermism and memo_nondet/{2,3} for creating memoised versions
of a binary predicate. The whole of this must be run inside ccstate:run_ref/1,
a context that provides mutable references as a control effect.
*/

:- use_module(library(rbutils)).
:- use_module(library(typedef)).
:- use_module(library(delimcc), [pr_reset/3, pr_shift/2]).
:- use_module(library(ccstate), [ref_new/2, ref_get/2, ref_app/2, ref_upd/3]).
:- use_module('../lambdaki').

:- meta_predicate memo_nondet(2,-), memo_nondet(2,-,-).
memo_nondet(P,Q) :- memo_nondet(P,Q,_).
memo_nondet(P, ccmemo:memf(P,R), ccmemo:memdump(R)) :-
   rb_empty(T),
   ref_new(T,R).

memdump(R,Memo) :-
   ref_get(R,T),
   rb_visit(T,Pairs),
   maplist(\ (K-entry(Vals,_))^(K-Vals)^true, Pairs,Memo).

memf(P,R,X,Y) :- pr_shift(nondet, mem(P,R,X,Y)).
choose(Xs,X) :- pr_shift(nondet, choose(Xs,X)).

:- meta_predicate guard(0).
guard(P) :- call(P) -> true; choose([],_).

%% run_list(+P:pred(A), -T:list(A)) is det.
:- meta_predicate run_list(1,-).
run_list(P,Result) :- pr_reset(nondet, to_list(P), Result).
to_list(P,[X]) :- call(P,X).

% choose(Xs:list(B),X:B): handler(list(A)).
choose(Xs,X,K,Ys) :- foldl(call_append(\X^K),Xs,[],Ys).

%% mem(+P:pred(+B,-C), +R:ref(memo(B,C)), +X:B, @Y:C, +K:pred(-list(A)), -T:list(A)) is det.
mem(P,R,X,Y,K,Ans) :-
   YK = \Y^K,
   ref_upd(R,Tab,Tab1),
   (  rb_trans(X, entry(Ys,Conts), entry(Ys,[YK|Conts]), Tab, Tab1)
   -> rb_fold(fst_call_append(YK),Ys,[],Ans)
   ;  rb_empty(EmptySet),
      rb_insert_new(Tab, X, entry(EmptySet,[]), Tab1),
      call(P,X,YNew),
      ref_app(R, rb_trans(X, entry(Ys,Conts), entry(Ys2,Conts))),
      (  rb_insert_new(Ys,YNew,t,Ys2)
      -> foldl(flip_call_append(YNew), [YK|Conts], [], Ans)
      ;  Ans=[], Ys2=Ys
      )
   ).

fst_call_append(Ky,Y-_,A1,A2) :- call_append(Ky,Y,A1,A2).
flip_call_append(Y,Ky,A1,A2) :- call_append(Ky,Y,A1,A2).
call_append(Ky,Y,A1,A2) :- call(Ky,Y,A), append(A,A1,A2).

