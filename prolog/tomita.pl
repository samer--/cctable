:- module(tomita, [start//0]).
/** <module> One of Tomita's example grammars
  Tomita grammar number 2 from Frost et al 2007. Works at part-of-speech level.
 */

:- use_module(library(dcg_core), [rep//2]).

:- table advm//0, adjm//0, nm//0, vc//0, np0//0, np1//0, np//0, pp//0, vp//0,
         s//0, dir//0, start//0.

start --> dir ; np ; s.

dir  --> dir, "c", dir ; pp, vp ; vp ; vp, pp.
advm --> "a", advm ; "a" ; advm, "c", advm.
adjm --> "j" ; "j", adjm ; advm, "j" ; adjm, "c", adjm.
nm   --> "n" ; "n", nm.
vc   --> "xv" ; "v".
np0  --> nm ; adjm, nm ; "d", nm ; "t", adjm, nm.
np1  --> adjm,  np0, pp, pp
       ; adjm, np0, pp
       ; adjm, np0
       ; np0, pp
       ; np0
       ; np0, pp, pp.
np   --> np, "c", np
       ; np1, "t", s
       ; np1, s
       ; np1.
pp   --> pp, "c", pp ; "p", np.
s    --> np, vp, pp, pp
       ; np, vp, pp
       ; np, vp
       ; s, "c", s.
vp   --> vc, np ; vp, "c", vp ; vc.

:- public sentence//1.
sentence(I) --> "nvdn", rep(I, "pdn").
