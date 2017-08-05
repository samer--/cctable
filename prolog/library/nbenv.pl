:- module(nbenv, [ run_nb_env/1, nb_get/2, nb_app/2, nb_new/2, nb_get_or_new/3, nb_app_or_new/3, nb_dump/1 ]).

:- use_module(library(rbutils)).

:- meta_predicate run_nb_env(0).
run_nb_env(Goal) :-
   rb_empty(Empty), assertion(\+nb_current(nbenv,_)),
   setup_call_cleanup(nb_setval(nbenv,Empty-0), Goal, cleanup).

cleanup :-
   debug(nbenv,'Deleting global variables.',[]),
   nb_getval(nbenv, KeyMap-_),
   rb_map(KeyMap, nb_delete),
   nb_delete(nbenv).

:- meta_predicate nb_app(+,2), nb_app_or_new(+,2,1), nb_get_or_new(+,1,1).
nb_dump(Map) :- nb_getval(nbenv,M-_), rb_map(M,nb_getval,Map).
nb_get(K,X)  :- nb_getval(nbenv,M-_), rb_lookup(K,R,M), nb_getval(R,X).
nb_app(K,P)  :- nb_getval(nbenv,M-_), rb_lookup(K,R,M), aux_app(P,R).
nb_new(K,Q)  :- nb_getval(nbenv,M1-I), rb_add(K,R,M1,M2), aux_new(Q,R,M2,I).
nb_get_or_new(K,Tgt,New) :-
   nb_getval(nbenv,M1-I),
   rb_upd_or_ins(K,What,M1,M2),
   (  What=update(R,R) -> nb_getval(R,X),  call(Tgt,X)
   ;  What=insert(R)   -> aux_new(New,R,M2,I)
   ).
nb_app_or_new(K,Upd,New) :-
   nb_getval(nbenv,M1-I),
   rb_upd_or_ins(K,What,M1,M2),
   (  What=update(R,R) -> aux_app(Upd,R)
   ;  What=insert(R)   -> aux_new(New,R,M2,I)
   ).

aux_app(P,R) :- nb_getval(R,X), call(P,X,Y), nb_setval(R,Y).
aux_new(New,R,M2,I1) :-
   atom_concat(nbenv,I1,R), I2 is I1+1,
   call(New,X), nb_setval(nbenv,M2-I2), nb_setval(R,X).

