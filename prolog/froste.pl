:- module(froste, [s//0, sm//0, goal_expls_tables/3, run_expl/2, (-)/2, sentence//1]).
/** <module> PRISM-like explanation building

   This shows how tabling can be augmented with explanation building
   _without_ modifying the underlying tabling system at all.

   Compared with modified tabling system of `ccprism`, there are two
   costs to this: firstly an extra layer of program transformation, 
   which is done manually below. Secondly, tabled predicates deliver
   duplicate solutions (with different explanations) to their consumers.

   Instead of tabling the target predicate directly, we table an
   auxilliary predicate ('..#') with an extra argument holding the explanation
   obtained by calling the target predicate ('..!') within a run_expl context.
   The explanation is discarded when adding the tabled call to the 
   explanation of the calling context. The end result is that each
   explanation of the target predicate results in a distinct solution
   of the auxilliary predicate.
*/


:- use_module(library(dcg_core), [rep//2, out//1]).
:- use_module(library(delimcc)).
:- use_module(library(ccstate)).
:- use_module(cctable_trie_kp_1p).

% --- machinery -----------

SW-V :- p_shift(expl, SW:=V).

:- meta_predicate sub(0).
sub(M:Goal) :- 
   copy_term(Goal,G1), 
   numbervars(G1,0,_), 
   p_shift(expl, M:G1).

:- meta_predicate run_expl(0,-).
run_expl(Goal, E) :- run_state_handler(expl, out, Goal, E, []).

:- meta_predicate goal_expls_tables(0,-,-).
goal_expls_tables(G,Es,Tabs) :- run_tabled(inner_goal_expls_tables(G,Es,Tabs)).
inner_goal_expls_tables(G,Es,['^top:top'-Es|TabsList]) :-
   setof(E,run_expl(G,E),Es),
   get_tables(Tabs), rb_visit(Tabs, TabsList).

% --- model manually transformed -----------

:- public sentence//1.
sentence(I) --> rep(I,"a").

s --> {s-1}, "a", s, s; {s-2}.

sm(S1,S2) :- cctabled('sm#'(S1,S2,_)), sub(sm(S1,S2)).
'sm#'(S1,S2,E) :- run_expl('sm!'(S1,S2), E).
'sm!' --> {sm-1}, "a", sm, sm, []; {sm-2}.

/* if we were automating the program transformation, we'd want to go from
   ==
   :- etable sm/2.
   sm --> {sm-1}, "a", sm, sm, []; {sm-2}.
   ==
   either directly to the above, or to this followed by ccmacros expansion
   ==
   :- table 'sm!'/3.
   sm(S1,S2) :- 'sm!'(S1,S2,_), sub(sm(S1,S2)).
   'sm!'(S1,S2,E) :- run_expl('sm&'(S1,S2), E).
   'sm&' --> {sm-1}, "a", sm, sm, []; {sm-2}.
   ==
*/
