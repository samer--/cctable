:- use_module(library(real)).
:- use_module(library(lambda)).
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

get_table(Exclude, Imps, Cases, Table) :-
   setof(\I^(imp_case_time(I,_,_), \+member(I,Exclude)), Imps1),
   sort_imps(Imps1,Imps2),
   maplist(nofail(rename_imp),Imps2, Imps),
   setof(\C^imp_case_time(_,C,_), Cases),
   maplist(\Case^Times^maplist(maybe_time(Case),Imps2,Times), Cases, Table).

sort_imps(Imps0, Imps1) :-
   include(in(Imps0),
           [yap,bprolog,desouter(plc),desouter(pl), cctable_db, cctable_db_kp, cctable_trie, cctable_trie_kp],
           Imps1).

nofail(P,X,Y) :- 
   call(P,X,Y) -> true;
   throw(failed(P,X)).

rename_imp(Imp, Imp0/kp) :- atom(Imp), atom_concat(Base, '_kp', Imp), !, rename_imp(Base,Imp0).
rename_imp(Imp, Imp0/'1p') :- atom(Imp), atom_concat(Base, '_1p', Imp), !, rename_imp(Base,Imp0).
rename_imp(cctable_trie, cctable(trie)).
rename_imp(cctable_db, cctable(db)).
rename_imp(cctable_mono, cctable(mono)).
rename_imp(desouter-X, desouter(X)).
rename_imp(X,X).

in(Xs,X) :- member(X,Xs).

maybe_time(Case, Imp, just(Time)) :- imp_case_time(Imp, Case, Time), !.
maybe_time(_, _, nothing).

lbr --> "\\\\", cr.
amp --> " & ".
@Cmd --> {atomic(Cmd)}, !, "\\", at(Cmd).
@Cmd --> {compound(Cmd), Cmd =.. [H|Args]}, !, "\\", at(H), seqmap(brace, Args).

tex_table(Exclude) -->
   { get_table(Exclude, Cols, Cases, Rows), length(Cols, NCols) },
   @newcommand, @plh, "[1]", brace((@lstinline, "[language=Prolog]{#1}")), cr,
   @begin("tabular", ("@{}l", rep(NCols, "r"), "@{}")), cr,
   amp, seqmap_with_sep(amp, head, Cols), lbr,
   @hline,
   seqmap_with_sep(lbr, tex_row, Cases, Rows), lbr,
   @hline,
   @end("tabular"), cr.

head(X) --> @rotatebox("90", @plh(wr(X))).
pl(X) --> brace(dq(wr(X))).
dq(X) --> "\"", phrase(X), "\"".
verb(X) --> "\\verb|", wr(X), "|".
math(X) --> "$", phrase(X), "$".

ranks(Times, Ranks) :-
   setof(T, member(just(T),Times), Sorted),
   maplist(rank(Sorted), Times, Ranks).

rank(_, nothing, inf).
rank(Sorted, just(T), N) :- nth1(N,Sorted,T).

tex_row(Case, Times) --> 
   {ranks(Times,Ranks)},
   pl(Case), amp, seqmap_with_sep(amp, num, Times, Ranks).

num(nothing, _) --> "--".
num(just(inf), _) --> !, math("\\times").
num(just(T), Rank) --> with_rank(Rank, fmt_time(T)).

with_rank(1, X) --> !, @textbf(X).
with_rank(2, X) --> !, @underline(X).
with_rank(_, X) --> phrase(X).

fmt_time(T) -->
   ( {T<1000} -> fmt('~0f',[T])
   ; {T<10000} -> {TS is T/1000.0}, fmt('~1f s',[TS])
   ; {TS is T/1000.0}, fmt('~0f s',[TS])
   ).

setof(Pred,Xs) :- setof(X,call(Pred,X),Xs).

case_pdf_file(C, File) :-
   format(string(File),'figs/~p.pdf',[C]).

refresh_table :-
   with_output_to_file('timings.tex', writedcg(tex_table([cctable_trie_1p, cctable_trie_kp_1p]))).
