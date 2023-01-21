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

open Idclass

type vid

(* terms now contain hash information. RB 26/i/00 *)
(* It's become an option so we don't cache terms which contain unknowns. RB 27/i/00 *)
(* I spent a lot of time trying to split Id and Unknown from this datatype. I retired 
   hurt. Ditto Collection. So don't think about it. RB 08-09/20
 *)
(* the bool in Subst is 'reducible'. Somehow a substitution generated from a selection 
   has false in that position: don't try to reduce it. Can't see how that happens, but 
   that's how it is. RB 2023/01/21
 *)
 
type term =
  | Id of (int option * vid * idclass)
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
  | Segvar of (int option * term list * term)
  | Element of (int option * resnum * term)

and resnum = Nonum | Resnum of int | ResUnknown of int

val _Resnum     : int -> resnum
val _ResUnknown : int -> resnum

val bracketed : term -> bool
val debracket : term -> term

val int_of_resnum : resnum -> int

val string_of_vid : vid -> string
val vid_of_string : string -> vid
