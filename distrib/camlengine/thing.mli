(* $Id$ *)

open Term.Funs
open Name
open Paraparam
open Proviso
open Sequent.Funs
open Tactic.Funs 
open Context.Cxt
open Mappingfuns

type thing =
    Rule of
      ((paraparam list * (bool * proviso) list * seq list * seq) * bool)
  | Theorem of (paraparam list * (bool * proviso) list * seq)
  | Tactic of (paraparam list * tactic)
  | Macro of (paraparam list * term)
type thingplace = InMenu of name | InPanel of name | InLimbo
val thingstring : thing -> string
val freshThingtoapply :
  bool -> name -> cxt -> term list ->
    (cxt * (term, term) mapping * (resnum list * resnum list) * thing)
      option
val freshThingtosubst :
  bool -> name -> cxt -> (term * term) list ->
    (cxt * (term, term) mapping * (resnum list * resnum list) * thing)
      option
val freshThingtoprove : name -> thing option
val freshGiven :
  bool -> seq -> cxt -> cxt * (resnum list * resnum list) * seq
val rearrangetoResolve : seq list -> seq -> seq list * seq
val instantiateRule :
  (term, term) mapping -> (bool * proviso) list -> seq list -> seq ->
    (term * term) list * (bool * proviso) list * seq list * seq
val compiletoprove :
  proviso list * seq list * seq -> (bool * proviso) list * seq list * seq
val formulageneralisable : paraparam list -> term -> bool
val addthing : name * thing * thingplace -> unit
val thingnamed : name -> (thing * thingplace) option
val thinginfo : name -> (thing * thingplace) option
(* including invisible provisos *)
   
val clearthings : unit -> unit
val thingnames : unit -> name list
val thingstodo : unit -> bool
type structurerule =
    CutRule
  | LeftWeakenRule
  | RightWeakenRule
  | IdentityRule
  | TransitiveRule
  | ReflexiveRule
val addstructurerule : structurerule -> name -> bool
val clearstructurerules : unit -> unit
val isstructurerule : structurerule -> name -> bool
val wehavestructurerule : structurerule -> string list option -> bool
val structurerulestring : structurerule -> string
val uniqueCut : unit -> name option
val isRelation : term -> bool
val numberrule : seq list * seq -> seq list * seq
val numberforproof : seq list * seq -> seq list * seq
val thingdebug : bool ref
val thingdebugheavy : bool ref
val autoAdditiveLeft : bool ref
val autoAdditiveRight : bool ref

exception Fresh_ of string list 
exception CompileThing_ of string list
