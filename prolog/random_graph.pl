:- module(random_graph, [shortest_path/3, path/4]).
% Benchmarks for mode-directed tabling (answer subsumption)

:- use_module(cctable_trie_kp_md).
:- use_module(ccmacros).

term_expansion(random_graph(G,V,E), [ :- discontiguous(graph/3)
                                    , :- discontiguous(edge/3)
                                    , graph(G,V,E)
                                    | Clauses ]) :-
   VSquared is V*V,
   format('Selecting ~w edges for random graph (~w) on ~w vertices...\n',[E,G,V]),
   randseq(E, VSquared, EdgeIndices),
   maplist(edge_index_to_edge(G, V), EdgeIndices, Clauses).

edge_index_to_edge(G, V, I, edge(G, V1, V2)) :-
   I0 is I - 1, divmod(I0, V, V1, V2).

random_graph(a, 1000, 1000).
random_graph(b, 2000, 2000).
random_graph(c, 4000, 4000).
% random_graph(d, 8000, 8000).
random_graph(a0, 200, 400).
random_graph(b0, 500, 1000).
% random_graph(aa, 1000, 4000).
% random_graph(bb, 2000, 8000).
% random_graph(cc, 4000, 16000).
% random_graph(dd, 8000, 32000).

:- table path(_, _, _, to(<)).
path(G,X,Y,1) :- edge(G,X,Y).
path(G,X,Y,N) :- path(G,X,Z,M), edge(G,Z,Y), N is M+1.

% :- table shortest_path(_, _, _, po(shorter)).
% shortest_path(G,X,Y,[]) :- edge(G,X,Y).
% shortest_path(G,X,Y,[Z|M]) :- shortest_path(G,X,Z,M), edge(G,Z,Y).

shorter(L1, L2) :- length(L1,N1), length(L2, N2), N1 < N2.
