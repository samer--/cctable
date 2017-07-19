:- module(lambdaki, [ (\)/1, (\)/2, (\)/3, (^)/3, (^)/4, (^)/5 ]).
% Very small lambda library (with no free variables and no checking anything)

:- meta_predicate \(0), \(1,?), \(2,?,?).
:- meta_predicate ^(?,0,?), ^(?,1,?,?), ^(?,2,?,?,?).

:- set_prolog_flag(generate_debug_info, false).

\(Lambda) :- copy_lambda(Lambda,Copy), call(Copy).
\(Lambda,A1) :- copy_lambda(Lambda,Copy), call(Copy,A1).
\(Lambda,A1,A2) :- copy_lambda(Lambda,Copy), call(Copy,A1,A2).

copy_lambda(M:Lam, M:Copy) :- copy_term(Lam,Copy).

^(A,B,A) :- call(B).
^(A,B,A,V1) :- call(B,V1).
^(A,B,A,V1,V2) :- call(B,V1,V2).
