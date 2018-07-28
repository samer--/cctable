:- module(random_graph, [edge/3]).

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
random_graph(a0, 200, 400).
random_graph(b0, 500, 1000).
