
:- use_module(random_graph).
:- use_module(cctable_trie_kp_md).
:- use_module(ccmacros).

:- table path_length(_, _, _, to(<)).
path_length(G,X,Y,1) :- edge(G,X,Y).
path_length(G,X,Y,N) :- path_length(G,X,Z,M), edge(G,Z,Y), N is M+1.

:- table path(_, _, _, po(shorter)).
path(G,X,Y,[]) :- edge(G,X,Y).
path(G,X,Y,[Z|M]) :- path(G,X,Z,M), edge(G,Z,Y).

shorter(L1, L2) :- length(L1,N1), length(L2, N2), N1 < N2.
min(X,Y,Z) :- X < Y -> Z=X; Z=Y.
