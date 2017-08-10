# Tabling implementations

Many variations. 

### Features

- *mono* means use a monolithic (single term) state.
- *env* means use a non-backtrackable environment to map variant classes to table entries
- *trie* means using trie for variant class map and solutions, and using lref for continuations.
- *db* means using dynamic database for state
- *lc* means use lambdas for representing continuations (incurs one copy per use)
- *kp* means put producer continuation in with the rest instead of passing to producer
- *1p* means single prompt implementation (can lead to unsafe interactions between multiple contexts).

### Implementations:

- cctable_lc
: Monolithic state. Very slow due to state copying.

- cctable_env_lc
: Factorise state by variant class using ccnbenv. Better.

- cctable_env
: Like cctable_env, but avoiding double copy of all continuations for each solution found.

- cctable_env_1p
: Like cctable_env_nl, but not using delimited control to provide non-backtrackable state, hence 1 prompt

- cctable_db_1p
: Using thread local dynamic predicates to factorise state completely, avoiding quadratic costs, also no lambda copy and 1 prompt

- cctable_db_kp_1p
: Using thread local dynamic predicates to factorise state completely, avoiding quadratic costs, also no lambda copy and 1 prompt
Smaller continuations by not passing producer continuation as argument.

- cctable_trie_1p
: Using SWI tries to store tables and solutions for each variant class, avoiding two quadratic costs.
Also using a reference to a mutable list to store consumer continuations instead of storing the list directly in the table entry,
avoiding another quadratic cost. Also 1 prompt.

- cctable_trie_kp_1p
: Like cctable_trie_nl_1p but including producer continuation with the list of consumer
continuations to avoid passing it as an argument to `producer/4`, since this
causes the size of the captured continuations to grow in long conjunctive chains
of tabled predicates.

Best so far is cctable_trie_kp_1p

