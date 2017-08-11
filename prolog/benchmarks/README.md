# Tabling benchmarks

The contents of this directory were derived from the Git repository
   https://github.com/JanWielemaker/tabling_benchmarks.git

## Usage

To run a single benchmark, choose one the runners from

	run-yap                      YAP built in tabling
	run-bp                       B-Prolog built in tabling
	run-swipl desouter {pl,plc}  Desouter et al's library ported to SWI. Two implementations,
	                             pl = direct port of original, all Prolog, on GitHub
										  plc = hybrid Prolog/C optimised port now included with SWI Prolog
	run-swipl cctable <imp>      One of the continuation based implementations in this repository, eg
                                cctable2, cctable6, cctable8 etc.

Then, call the runner from the shell prompt as follows:

	$ ./<runner> <show_results?:{0,1}> <benchmark_file:filename> <size:int>

For example,

	$ ./run-swipl desouter plc 1 fib.pl 50
	$ ./run-yap 0 naiveReverse.pl 1000

The first prints the results, the second does not. When printing results,
Benchmarks with large result sets simply print the number of solutions, not
the actual results.

To run all the benchmarks, use `bench` or minibench` for a smaller set.

	$ ./bench "<runner>" <show_results?:{0,1}> <results-folder>

The results folder will be created if necessary.
Eg

	$ ./bench run-yap 0 results/full
	$ ./minibench "run-swipl cctable cctable2" 0 results/mini

