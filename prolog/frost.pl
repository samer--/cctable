:- module(frost, [s//0, sm//0, sml//0, smml//0]).
/** <module> Highly ambiguous grammars, from Frost et al. */
:- use_module(library(dcg_core), [rep//2]).

:- table sm//0, sml//0, smml//0, aux//0.

s --> "a", s, s; [].
sm --> "a", sm, sm; [].
sml --> sml, sml, "a"; [].
smml --> smml, aux; [].
aux --> smml, "a".

:- public sentence//1.
sentence(I) --> rep(I,"a").
