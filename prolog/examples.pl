:- module(examples, [fib/2, fib_clp/2, tom_twice/2, pathl//0, pathr//0, silly//0]).
/** <module> Some tables predicates and grammars */

:- use_module(library(clpfd)).

:- table fib/2, fib_clp/2.
fib(0,1).
fib(1,1).
fib(N,X) :-
   succ(M,N), fib(M,Y),
   succ(L,M), fib(L,Z),
   X is Y + Z.

fib_clp(0,1).
fib_clp(1,1).
fib_clp(N,X) :-
   M #= N-1, L #= M-1, L #>= 0, X #= Y+Z,
   fib_clp(M,Y), fib_clp(L,Z).

edge(a,b).
edge(a,c).
edge(b,d).
edge(c,d).
edge(d,e).
edge(d,f).
edge(f,g).

% Tom's example
:- table tom/1.

tom(X) :- (X=a; X=b), writeln(tom(X)).

tom_twice(X,Y) :- tom(X), writeln(middle:X), tom(Y), writeln(end:Y).

% four tabled transitive closures of edge/2
:- table pathl//0, pathl1//0, pathr//0, pathr1//0.
pathl  --> edge; pathl, edge.
pathl1 --> pathl1, edge; edge.
pathr  --> edge; edge, pathr.
pathr1 --> edge, pathr1; edge.

% mutual left recursion
:- table pathml//0, aux//0.
pathml --> aux, edge; edge.
aux --> pathml.

% for testing handling of input and output variables in continuations.
path_a(Y) :- pathl(a,X), Y=a(X).
path1_a(Y) :- pathl1(a,X), Y=a(X).

% specify preterminals as DCG that spits out alternatives
user:term_expansion(Lab | Body, Clause) :-
   dcg_translate_rule(Lab --> Body, Clause).

%% +(Preterm)// is nondet.
%  Nonterminal for a pre-terminal -- Preterm is expected to be a DCG
%  goal that generates a list of alternative terminals. Then, one of
%  these terminals is emitted nondeterministically.
:- meta_predicate +(2,?,?).
+Lab --> [T], {call(Lab,Vals,[]), member(T,Vals)}.

:- table s//0, np//0, vp//0, pp//0, nom//0.

silly --> s.
s --> np, vp.

np --> +d, nom
     ; +pn
     ; np, pp.

vp --> +iv
     ; +tv, np
     ; +dv, np, np
     ; vp, pp
     ; +mv, s.

nom --> +n
      ; +adj, nom.

pp --> +p, np.

% preterminal switch declarations
adj | [hot,cold,thin,fat,disgusting,lovely].
pn  | [alice, bob, cuthbert, delia, edna].
d   | [the,a,some,my]. % ,every,no].
mv  | [knew,thought,believed,said].
dv  | [gave,made,baked].
tv  | [saw, ate, hated, baked, liked, walked, ran, loved, caught].
iv  | [lived, worked].
n   | [dog,telescope,man,cat,mat,cake,box,floor,face,pie,moose,pyjamas,park].
p   | [with,on,under,in,without,by].
