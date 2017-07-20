## Memoisation of binary predicates in functional style

This is a more or less straight translation of the functional style continuation
based memoisation presented in [1], which was in turn based on the work in [2].
You must install the SWI Prolog add-on package `genutils` to run this code. At
the SWI Prolog prompt, do

   ?- pack_install(genutils).

Only binary predicates with one input and one output argument can be memoised.
This is done using `memo_nondet/2` (or `memo_nondet/3` to get access to the memo
tables). This yields a callable term representing the memoised predicate.

All nondetermism is represented using the predicate `choose/2`, which takes a list
of alternatives (which can be empty to represent faiure) and returns one of these
nondeterministically. This computational effect is handled using delimited control,
and eventually reified using lists to represent alternative results. All of this is
done without using Prolog's ordinary nondeterminism. In fact, you cannot use Prolog's
nondeterminism within this framework - the whole thing will just break.

You can test the system by loading `test_memo.pl` into SWI Prolog. The simple
test predicates can be run inside `run_list_ref/1` to provide both the required
contexts for mutable references and nondeterministic memoisation, for example:

	test_memo: ?- run_list_ref(test2, Ans).
   Ans = [a(1), b(1), c(1), a(2), b(2), c(2), a(3), b(3), c(...)].

The more complicated examples have test predicates which provide the context
for nondeterminism, but not for references, which you must provide when you call them:

   test_memo:  ?- run_ref(test_path(left, a, Ends)).
   Ends = [b, c, d, e, g, h, f].

Alternatively, you can run a whole interactive Prolog shell within the context
of `run_ref/1` by using `ccshell/0` from `library(delimcc)`. Once in there, you
can run the various tests without calling `run_ref/1` again, eg

   test_memo:  ?- run_ref(ccshell).
   test_memo: [1]  ?- run_list(test1, Ans).
   Ans = [r(1), r(2), r(3)].

   test_memo: [1]  ?- test_path(left,a,Ends).
   Ends = [b, c, d, e, g, h, f].

   test_memo: [1]  ?- test_grammar(m(sml),12).
   % 140,173 inferences, 0.073 CPU in 0.077 seconds (95% CPU, 1912424 Lips)
   true.

The [1] indicates that you are in a nested shell.
Note, however, that old references are not garbage collected and will not be
released until you exit the nested shell by typing Control-D.

