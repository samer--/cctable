:- module(random_graph, [edge/3]).

term_expansion(random_graph(G,Seed,V,E), Clauses) :-
   Clauses = [ :- discontiguous(graph/3)
             , :- discontiguous(edge/3)
             , graph(G,V,E) | Edges ],
   VSquared is V*V,
   format('Selecting ~w edges for random graph (~w) on ~w vertices...\n',[E,G,V]),
   setrand(Seed),
   randseq(E, VSquared, EdgeIndices),
   maplist(edge_index_to_edge(G, V), EdgeIndices, Edges).

edge_index_to_edge(G, V, I, edge(G, V1, V2)) :-
   I0 is I - 1, divmod(I0, V, V1, V2).

random_graph(a, rand(1,20,50),      1000, 1000).
random_graph(b, rand(1023,234,564), 2000, 2000).
random_graph(a0, rand(852,23,7543), 200, 400).
random_graph(b0, rand(874,238,235), 500, 1000).
random_graph(c0, rand(121,555,888), 50, 100).
random_graph(d0, rand(724,197,633), 10, 20).

edge(0,X,Y) :- edge(X,Y).
edge(a,b).
edge(a,c).
edge(b,d).
edge(c,d).
edge(d,e).
edge(d,f).
edge(f,g).
edge(d,a).
edge(g,e).
edge(c,e).
