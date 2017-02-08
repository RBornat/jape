(*
    Copyright (C) 2003-17 Richard Bornat & Bernard Sufrin
     
        richard@bornat.me.uk
        sufrin@comlab.ox.ac.uk

    This file is part of the jape proof engine, which is part of jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).

*)

(* don't open this freely.  It's meant to be a bit abstract, but it's
 * here because some people need to see it.
 * RB
 *)

open Termtype

type proviso = 
  | FreshProviso of (bool * bool * bool * term)
  | UnifiesProviso of (term * term)
  | NotinProviso of (term * term)
  | NotoneofProviso of (term list * term * term)
  | DistinctProviso of term list

(* Meaning of provisos at present :
   FreshProviso (h, g, r, v)     : Variable v doesn't occur free in hypotheses (if h), 
                                   conclusions (if g); non-principal formulae only (if r)
   UnifiesProviso (t1, t2)       : t1 must unify with t2.
   NotinProviso (v, t)           : variable v must not occur free in t
   DistinctProviso vs            : the variables vs are distinct. Equivalent to n*(n-1) 
                                   NotinProvisos
   NotoneofProviso (vs, pat, _C) : in any element of collection _C that matches pat,
                                   variables vs don't occur in the places indicated
                                   by pat.
 *)
                      

let _FreshProviso bbbt      = FreshProviso bbbt
let _UnifiesProviso tt      = UnifiesProviso tt
let _NotinProviso vt        = NotinProviso vt
let _DistinctProviso vs     = DistinctProviso vs
let _NotoneofProviso vspatc = NotoneofProviso vspatc
