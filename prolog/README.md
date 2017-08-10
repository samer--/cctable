## Tabling implementations

Many variations. Summary of implementation strategies and features:

- cctable
: Monolithic state. Very slow due to state copying.

- cctable_env
: Factorise state by variant class using ccnbenv. Better.

- cctable_env_nl
: Like cctable_env, but avoiding double copy of all continuations for each solution found.

- cctable_env_nl_1p
: Like cctable_env_nl, but not using delimited control to provide non-backtrackable state, hence 1 prompt

- cctable_db
: Using thread local dynamic predicates to factorise state completely, avoiding quadratic costs, also no lambda copy and 1 prompt

- cctable5
for adding solutions.

- cctable_trie
: Using SWI tries to store tables and solutions for each variant class, avoiding two quadratic costs.
Also using a reference to a mutable list to store consumer continuations instead of storing the list directly in the table entry,
avoiding another quadratic cost. Also 1 prompt.

- cctable_trie_kp
: Like cctable_trie but including producer continuation with the list of consumer
continuations to avoid passing it as an argument to `producer/4`, since this
causes the size of the captured continuations to grow in long conjunctive chains
of tabled predicates.

Best so far is cctable_trie_kp

