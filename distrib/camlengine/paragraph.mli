(* $Id$ *)

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
  | Menu of (name * menupara list)
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

