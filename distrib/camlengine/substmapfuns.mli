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

open Answer
open Facts
open Termtype
 
(* Oh at this point I wish for a type which distinguishes
   variables from terms.  Even though it might be a pain everywhere
   else. RB
 *)
     
val substdebug : bool ref
val varmappedbyq : facts -> term -> (term * term) list -> answer
val varboundbyq : facts -> term -> term list -> answer
val varoccursinq : facts -> term -> term -> answer
val varmappedto : facts -> term -> (term * term) list -> term option
val simplifySubstAnyway : facts -> (term * term) list -> term -> term
val simplifySubst : facts -> (term * term) list -> term -> term option
val simplifysubstmap :
  facts -> term -> (term * term) list -> (term * term) list option
val substmapdom : (term * term) list -> term list
val substmapran : (term * term) list -> term list
val restrictsubstmap :
  facts -> (term * term) list -> term list -> term list ->
    (term * term) list option
val plussubstmap :
  facts -> (term * term) list -> (term * term) list ->
    (term * term) list option
(* some useful survivors *)
val vtmetacount : (term * term) list -> int
val vtminus :
  facts -> (term * term) list -> (term * term) list -> (term * term) list
val vtsplit :
  facts -> (term * term) list -> term list ->
    (term * term) list * (term * term) list * (term * term) list
