(*
    $Id$

    Copyright (C) 2003-8 Richard Bornat & Bernard Sufrin
     
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

type vid = string (* but nobody else knows *) 
 and idclass = Idclass.idclass

(* terms now contain hash information. RB 26/i/00 *)
(* It's become an option so we don't cache terms which contain unknowns. RB 27/i/00 *)
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
and element =
    Segvar of (int option * term list * term)
  | Element of (int option * resnum * term)
and resnum = Nonum | Resnum of int | ResUnknown of int

let rec int_of_resnum =
  function
    Resnum n -> n
  | ResUnknown n -> n
  | Nonum -> 0

(* We keep the user's bracket structure, so every time we match/unify/compare
   two terms, we must debracket them
 *)
let rec debracket =
  function
    Fixapp (_, ["("; ")"], [t]) -> debracket t
  | t -> t
let rec bracketed =
  function
    Fixapp (_, ["("; ")"], [t]) -> true
  | t -> false

let vid_of_string s = s
let string_of_vid v = v
