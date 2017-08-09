## Tabling implementations

Many variations. Summary of implementation strategies and features:

- cctable0
: Monolithic state. Very slow due to state copying.

- cctable
: Factorise state by variant class. Better.

- cctable1
: Like cctable, but factorising solutions and continuations list in state.
Not very effective as it does not address quadratic cost of adding solutions or consumers.

- cctable2
: Like cctable, but avoiding double copy of all continuations for each solution found.

- cctable3
: Like cctable2, but not using delimited control to provide non-backtrackable state.

- cctable4
: Using thread local dynamic predicates to factorise state completely, avoiding quadratic costs.

- cctable5
: Using SWI tries to store tables and solutions for each variant class, avoiding quadratic cost
for adding solutions.

- cctable6
: Like cctable5, but using a reference to a mutable list to store consumer
continuations instead of storing the list directly in the table entry. Avoids
quadratic cost of adding continuations to consumer list.

- cctable7
: Like cctable6, but using nb_set instead of trie to collect solutions.

- cctable8
: Like cctable6 but including producer continuation with the list of consumer
continuations to avoid passing it as an argument to `producer/4`, since this
causes the size of the captured continuations to grow in long conjunctive chains
of tabled predicates.

Best so far is cctable8.

