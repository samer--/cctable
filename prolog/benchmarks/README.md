# Tabling benchmarks

The contents of this directory were derived from the Git repository
   https://github.com/JanWielemaker/tabling_benchmarks.git

## Usage

To run a single benchmark, choose one the runners from

	run-yap       YAP built in tabling
	run-bp        B-Prolog built in tabling
	run-desouter  Desouter et al's Prolog library ported to SWI
	run-swipl     SWI Prolog's tabling library adapted from Desouter et al
	run-cctable   The continuation based library in this repository

Then, call the runner from the shell prompt as follows:

	$ ./<runner> <show_results?:{0,1}> <benchmark_file:filename> <size:int>

For example,

	$ ./test-swipl 1 fib.pl 50
	$ ./test-yap 0 naiveRevers.pl 1000

The first prints the results, the second does not. When printing results,
Benchmarks with large result sets simply print the number of solutions, not
the actual results.

To run all the benchmarks, use `run`

	$ ./run <runner> <show_results?:{0,1}> 

Eg

	$ ./run run-yap 0
	$ ./run run-swipl 0

