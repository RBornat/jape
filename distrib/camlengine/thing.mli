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

open Termtype
open Name
open Paraparam
open Proviso
open Seqtype
open Tactictype 
open Cxttype
open Mappingfuns

type thing =
  | Rule    of ((paraparam list * (bool * proviso) list * seq list * seq) * bool)
  | Theorem of (paraparam list * (bool * proviso) list * seq)
  | Tactic  of (paraparam list * tactic)
  | Macro   of (paraparam list * term)
  
type thingplace = InMenu of name | InPanel of name | InLimbo

val string_of_thing : thing -> string
val freshThingtoapply : bool -> name -> cxt -> term list -> (name -> bool)
                     -> (cxt * (term, term) mapping * (resnum list * resnum list) * thing) option
val freshThingtosubst : bool -> name -> cxt -> (term * term) list -> (name -> bool) 
                     -> (cxt * (term, term) mapping * (resnum list * resnum list) * thing) option
val freshThingtoprove : name -> thing option
val freshGiven : bool -> seq -> cxt -> (name -> bool) -> cxt * (resnum list * resnum list) * seq
val rearrangetoResolve : seq list -> seq -> seq list * seq
val instantiateRule : (term, term) mapping -> (bool * proviso) list -> seq list -> seq
                   -> (term * term) list * (bool * proviso) list * seq list * seq
val compiletoprove : paraparam list * proviso list * seq list * seq -> (bool * proviso) list * seq list * seq
val formulageneralisable : paraparam list -> term -> bool

val addthing : name * thing * thingplace -> unit
val thingnamed : name -> (thing * thingplace) option
val thinginfo : name -> (thing * thingplace) option (* including invisible provisos *)
   
val clearthings : unit -> unit
val thingnames : unit -> name list
val thingstodo : unit -> bool

type structurerule = CutRule
                   | LeftWeakenRule
                   | RightWeakenRule
                   | IdentityRule
                   | TransitiveRule
                   | ReflexiveRule
val addstructurerule : structurerule -> name -> bool
val clearstructurerules : unit -> unit
val isstructurerule : structurerule -> name -> bool
val wehavestructurerule : structurerule -> string list option -> (name -> bool) -> bool
val string_of_structurerule : structurerule -> string
val uniqueCut : unit -> name option

val isRelation : term -> bool

val numberrule : seq list * seq -> seq list * seq
val numberforproof : seq list * seq -> seq list * seq

val thingdebug : bool ref
val thingdebugheavy : bool ref

exception Fresh_ of string list 
exception CompileThing_ of string list
