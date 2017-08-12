:- use_module(library(real)).
:- use_module(library(fileutils)).
:- use_module(library(data/pair)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).
:- use_module(jd(datasets/prolog/vis)).

:- multifile imp_case_time/3.

loadall :- 
   forall(find_files(in('results/pl/holly','*'), F),
			 (writeln(loading:F), load_files([F],[]))).

:- initialization loadall.

inf_to_nan(inf, 1.5NaN) :- !.
inf_to_nan(X, X).

x(A,B,C) :-
   imp_case_time(A,B,C1),
   inf_to_nan(C1,C).

zip(X,Y,Z) :- maplist(pair,X,Y,Z).
recip(K,A,X,Y) :- Y is K/(X+A).
const(X,_,X).


plot_comparison(Orient, C, Exclude) :-
   plot_comparison(Orient, const('out.pdf'), C, Exclude).

plot_comparison(vert, CaseToFile, C, Exclude) :-
   setof(I-T, (x(I,C,T), \+member(I,Exclude)), D),
   term_string(C,Title), 
   call(CaseToFile, C, File),
   zip(Cs,Ts,D),
   maplist(recip(1000.0,1.0),Ts,Qs),
   max_list(Qs,Max),
   print_fig(pdf, barplot(Title,Cs,Qs,[ylim=[0,Max],las=2]), File, [size(8,16)]).

plot_comparison(horiz, CaseToFile, C, Exclude) :-
   setof(I-T, (x(I,C,T), \+member(I,Exclude)), D),
   term_string(C,Title), 
   call(CaseToFile, C, File),
   zip(Cs,Ts,D),
   maplist(recip(1000.0,1.0),Ts,Qs),
   max_list(Qs,Max),
   print_fig(pdf, (font_size(8), 
                   margins([bottom(2.5), left(6), top(1.5), right(2.5)]),
                   barplot(Title,Cs,Qs,[xlim=[0,Max],horiz='TRUE',las=1])),
             File, [size(10,5.5)]).

tex_table(C, Exclude) -->
   { setof(I-T, (x(I,C,T), \+member(I,Exclude)), D),
      zip(Cs,Ts,D),
      maplist(recip(1000.0,1.0),Ts,Qs) 
   }.

case_pdf_file(C, File) :-
   format(string(File),'figs/~p.pdf',[C]).
