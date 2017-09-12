:- module(frost, [s//0, sm//0, sml//0, smml//0
                 ,s//1, sm//1, sml//1, smml//1
                 ,print_tree/1]).
/** <module> Highly ambiguous grammars, from Frost et al. */
:- use_module(library(dcg_core), [rep//2]).
:- use_module(library(data/tree),[]).
:- use_module(library(typedef)).

:- table sm//0, sml//0, smml//0, aux//0.
s --> "a", s, s; [].
sm --> "a", sm, sm; [].
sml --> sml, sml, "a"; [].
smml --> smml, aux; [].
aux --> smml, "a".

:- public sentence//1.
sentence(I) --> rep(I,"a").

/* Now with semantics (parse trees).
 * Without careful handling, this would mess up the efficiency of
 * the tabled process, because we would get a distinct solution
 * for each explanation of a given non-terminal. However, by doing
 * some funky stuff in subgoal//2 instead of returning the semantic
 * term as is, we should get some subtree sharing, similar to that
 * in Frost et al's implementation. However, there is still some
 * extra redundancy in that a nonterminal can succeed more than
 * once with the same tail sequence but with different parses,
 * even thought the parse is discarded by subgoal/4.
 */

:- type tree(L,T) ---> +T; e; g(goal,s,s); L / list(tree(T)).

seq(Gs,Ss) --> foldl(subgoal,Gs,Ss).
subgoal(G,g(G,I,J),I,J) :- call(G,_,I,J). 
% subgoal(G,S,I,J) :- call(G,S,I,J). % Full tree, no sharing. Bad.

:- table sm//1, sml//1, smml//1, aux//1.

s(s/S) --> seq([a, s, s],S); empty(S).
sm(sm/S) --> seq([a, sm, sm],S); empty(S).
sml(sml/S) --> seq([sml, sml, a],S); empty(S).
smml(smml/S) --> seq([smml, aux],S); empty(S).
aux(aux/S) --> seq([smml, a],S).
empty([e]) --> [].
a(+a) --> "a".

print_tree(T) :- convert_tree(T,T1), tree:print_tree('',T1).
convert_tree(e,node(e,[])).
convert_tree(+T,node(+T,[])).
convert_tree(g(G,S1,S2),node(g(G,S1,S2),[])).
convert_tree(L/Ts,node(L,Ts1)) :- maplist(convert_tree,Ts,Ts1).

% version of foldl1 which provides final state to final call.
% This has an effect on which tabled variants are called when
% used in seq//2 above instead of foldl//3, and makes it more like
% the versions without parse trees.
foldl1(_,[],[]) --> [].
foldl1(P,[X],[Y]) --> call(P,X,Y).
foldl1(P,[X1,X2|Xs],[Y1,Y2|Ys]) --> call(P,X1,Y1), foldl1(P,[X2|Xs],[Y2|Ys]).
