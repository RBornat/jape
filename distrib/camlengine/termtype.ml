(*
    Copyright (C) 2003-19 Richard Bornat & Bernard Sufrin
     
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

(* I tried, for about three weeks, to remake the term datatype (should be the formula datatype, really)
   so that Id and Unknown on the one part, and Collection on the other, were separated. I retired hurt
   (but see the remaking_term_datatype branch on github). I did (re)learn a lot about how things work.
   RB 09/20
 *)
type vid = string (* but nobody else knows *) 
 and idclass = Idclass.idclass

(* terms now contain hash information. RB 26/i/00 *)
(* It's become an option so we don't cache terms which contain unknowns. RB 27/i/00 *)

(* the bool in Subst is 'reducible'. Somehow a substitution generated from a selection 
   has false in that position: don't try to reduce it. Can't see how that happens, but 
   that's how it is. RB 2023/01/21
 *)
 
type term =
    Id of (int option * vid * idclass)
  | Unknown of (int option * vid * idclass)
  | App of (int option * term * term)
  | Tup of (int option * string * term list)
  | Literal of (int option * litcon)
  | Fixapp of (int option * string list * term list)
  | Subst of (int option * bool * term * (term * term) list)
  | Binding of
      (int option * (term list * term list * term list) *
         (term * (int * int)) list * term)
  | Collection of (int option * idclass * element list)

and litcon = Number of int | String of string

(* a Segvar has a list of prefix operators and a variable. RB 09/20 *)
and element =
  | Segvar of (int option * term list * term)
  | Element of (int option * resnum * term)

and resnum = Nonum | Resnum of int | ResUnknown of int

let _Resnum     i = Resnum i
let _ResUnknown i = ResUnknown i

let int_of_resnum =
  function
  | Resnum n -> n
  | ResUnknown n -> n
  | Nonum -> 0

(* We keep the user's bracket structure, so every time we match/unify/compare
   two terms, we must debracket them
 *)
let rec debracket =
  function
  | Fixapp (_, ["("; ")"], [t]) -> debracket t
  | t -> t
let rec bracketed =
  function
  | Fixapp (_, ["("; ")"], [t]) -> true
  | t -> false

let vid_of_string s = s
let string_of_vid v = v
