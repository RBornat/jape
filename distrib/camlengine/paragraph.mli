(*
	$Id$

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

open Term.Funs
open Sequent.Funs
open Proviso
open Paraparam
open Tactic
open Menu
open Panelkind
open Doubleclick
open Forcedef
open Name
open Proofstage

type ruleheading = RuleHeading of (name * paraparam list * proviso list)

type tacticheading = TacticHeading of (name * paraparam list)

type paragraph =
    AutoRule of (bool * tactic list)
  | Conjecture of (ruleheading * seq)
  | File of (string * paragraph list)
  | FontSpec of string
  | ForceDef of (term * forcedef)
  | HitDef of (dclick * tactic * seq)
  | InitVar of (name * term)
  | MacroDef of (tacticheading * term)
  | Menu of (bool * name * menupara list)
  | Panel of (name * panelpara list * panelkind)
  | Proof of
      (name * proofstage * seq *
         (seq list * paraparam list * proviso list * tactic) *
         (seq * model) option)
  | RuleDef of (ruleheading * seq list * seq * bool)
  | RuleGroup of (ruleheading * paragraph list)
  | StructureRule of (string * name)
  | TacticDef of (tacticheading * tactic)
  | Theory of (ruleheading * paragraph list)

and panelpara = Panelstuff of paneldata | Panelpara of paragraph

and menupara = Menustuff of menudata | Menupara of paragraph

exception Use_

val file2paragraphs :
  (string list -> unit) ->
    (string list * string * string * int -> bool) -> string ->
    paragraph list
val string2paragraph :
  (string list -> unit) ->
    (string list * string * string * int -> bool) -> string -> paragraph

