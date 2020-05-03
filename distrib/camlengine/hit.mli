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

open Termtype

(* Hits are what drawing packages (treedraw, boxdraw, and one day etc.) translate
 * clicks into.  They are also used to locate text selections.  Only conclusions are
 * treated with the transitivity transformation, so only conclusion clicks can be
 * sided.  
 *
 * Sels are what we translate collections of Hits into.
 * 
 * The interface ('server') is never supposed to report an ambiguous click, but 
 * a text selection in an ambiguous formula must be reported somehow, so we can't
 * get rid of AmbigHit as I would like.
 *
 * ''a is a path -- there are various kinds.
 *)

type 'a hit = FormulaHit of 'a fhit | ReasonHit of 'a

and 'a fhit =
  | ConcHit of ('a * (element * side option))
  | HypHit of ('a * element)
  | AmbigHit of (('a * (element * side option)) * ('a * element))

and side = Left | Right

(* In box display at least, a single displayed element may play more than one 
 * role. hitkind (should perhaps be hitpathkind) allows us to choose which we want.
 *)

type hitkind = HitPath | PrunePath | LayoutPath

(* Selections uniquely identify a single sequent.  
 * They are either conclusion+hypotheses, reason or nothing at all.
 * At present I countenance either a single conclusion selection, or none.
 *
 * Interfaces are supposed to sort out the user's clicks so that it's reasonable
 * to say that either a reason is selected, or some formulae are selected, or neither.
 *
 * Text selections are rolled up into formula selections, because why not?
 *
 * We distinguish 'text selection only' selections.
 *)

type 'a sel =
  | FormulaSel of
      ( 'a
      * (element * side option) option
      * element list
      * ('a * (element * side option) * string list) list
      * ('a * element * string list) list
      * string list )
  | TextSel of (('a fhit * string list) list * string list)
  | ReasonSel of 'a

val fhitpath : 'a fhit -> 'a option

val string_of_fhit : ('a -> string) -> 'a fhit -> string

val string_of_hitkind : hitkind -> string

val hitpath : 'a hit -> 'a option

val string_of_hit : ('a -> string) -> 'a hit -> string

val selpath : 'a sel -> 'a option

val string_of_sel : ('a -> string) -> 'a sel -> string

val string_of_side : side -> string

val tranfhitpath : ('a -> 'b) -> 'a fhit -> 'b fhit

val tranhitpath : ('a -> 'b) -> 'a hit -> 'b hit

val transelpath : ('a -> 'b) -> 'a sel -> 'b sel
