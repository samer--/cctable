/* Modified version of tabling_testlib.pl from SWI Prolog source.
   Adapated to work with cctabling library.
   Samer Abdallah, 2017.

   Original header follows.
   ------------------------------------------------------------------

   Part of SWI-Prolog

    Author:        Benoit Desouter <Benoit.Desouter@UGent.be>
		   Jan Wielemaker (SWI-Prolog port)
    Copyright (c)  2016, Benoit Desouter
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(cctable_testlib,
	  [ test_expected_variants_present/2,
	    test_answers_expected_tables/3,
	    compare_real_expected_answers/3,		% :Name, +Arity, :E
       mqualify/3
	  ]).

:- use_module(library(dialect/hprolog)).

:- meta_predicate compare_real_expected_answers(:,+,1).

test_expected_variants_present(Expected, Tables) :-
	maplist(test_expected_variants_present_(Tables,True),Expected),
	True \== false,
	% now all expected variants are present.
	% next, we check whether there aren't any more present.
	length(Expected,NumExpected),
	length(Tables,NumActual),
	assert_equal(NumExpected,NumActual,'test_expected_variants_present').

mqualify(M,T,M:T).

test_expected_variants_present_(Tables,_,X) :- member(X-_, Tables), !.
test_expected_variants_present_(_,True,X) :-
	print_message(error, format('Missing table for variant ~p',[X])),
	True = false.

test_answers_expected_tables(Expected, Answers, Tables) :-
	maplist(test_answers_expected_tables_(Tables,True), Expected, Answers),
	True \== false.

test_answers_expected_tables_(Tables, _, Variant, Answers) :-
	test_answers_for_expected_variant(Tables, Variant, Answers), !.
test_answers_expected_tables_(_, True, Variant, _Answers) :-
	print_message(error, format('Wrong answers for expected variant ~p', [Variant])),
	True = false.

% ATTENTION: works only for ground answers in the tables (which we currently enforce when adding answers as well). To be on the safe side, an exception will be thrown if one of the expected answers is nonground.
test_answers_for_expected_variant(Tables, Variant, ExpectedAnswers) :-
   member(Variant-Answers, Tables),
	maplist(test_answers_for_variant_(Answers, True), ExpectedAnswers),
	True \== false,
	% Now check that there are not more answers than expected
	length(ExpectedAnswers,NumExpected),
	length(Answers,NumActual),
	assert_equal(NumExpected,NumActual,'test_answers_for_expected_variant').

test_answers_for_variant_(Answers, True, ExpectedAnswer) :-
	(   ground(ExpectedAnswer) ->  true
	;   print_message(error, format('Got nonground expected answer ~p, \c
					 which it cannot handle correctly',
					[ ExpectedAnswer ])),
	    True = false
	),
	% get_answer => uses unification, so this won't work properly
	% for nonground answers.
	(   member(ExpectedAnswer, Answers) ->  true
	;   print_message(error, format('Missing expected answer ~p',
					[ExpectedAnswer])),
	    True = false
	).

assert_equal(NumExpected,NumActual,ContextualInfo) :-
	(   NumExpected == NumActual
	->  true
	;   print_message(error,
			  format('assert_equal failed in context of ~w: \c
			  expected ~w but was ~w~n',
				 [ContextualInfo,NumExpected,NumActual])),
	    fail
	).


% G = Name of goal to obtain real answers. Should take A free variables.
% A = Arity of goal to obtain real answers.
% E = Name of goal to obtain list of expected answers. Should take one argument.
compare_real_expected_answers(M:G,A,E) :-
  call(E,E2),
  length(FreeVarsList,A),
  G2 =.. [G|FreeVarsList],
  list_to_tuple(FreeVarsList,FreeVarsTuple),
  findall(FreeVarsTuple,M:G2,R),
  expect_same_size(E2,R,Ok1),
  expect_lists_equal_sets(E2,R,Ok2),
  Ok1 == true, 
  ( Ok2 == true -> true
  ; print_message(error, format('In ~w',[M:G])),
    fail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% E = list that has the expected number of elements
% A = list that has the actual number of elements
expect_same_size(E,A,Result) :-
  length(E,Es),
  length(A,As),
  (   Es == As
  ->  Result = true
  ;   print_message(error,
		    format('expected list to have ~d elements but it had ~d~n',
			   [Es,As])),
      Result = false
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Both lists are turned into sets first!
% E = list having expected elements
% A = list having actual elements
expect_lists_equal_sets(E,A,True) :-
  list_to_set(E,Es),
  list_to_set(A,As),
  (   (   list_difference_eq(As,Es,[]),
	  list_difference_eq(Es,As,[])
      )
  ->  True = true
  ;   print_message(error,
		    format('lists do not represent equal sets. \c
		            Expected list was ~p, actual list was ~p',[E,A]))
  ).

% [X,Y,Z] -> X-Y-Z
% Empty list. No sensible behaviour. Throw exception.
list_to_tuple([],_) :- !,
  type_error(non_empty_list, []).
% List with at least one element.
list_to_tuple([First|Rest],Tuple) :-
  foldl(to_tuple,Rest,First,Tuple).

% E = 'element'
% Ai = 'accumulator in'
to_tuple(E,Ai,Ai-E).
