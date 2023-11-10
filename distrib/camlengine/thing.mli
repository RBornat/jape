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
open Sequent
open Name
open Structurerule
open Paraparam
open Proviso
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

val storableprovisos : visproviso list -> (bool * proviso) list (* with invisibles stripped out *)
val addthing : name * thing * thingplace -> unit
val thingnamed : name -> (thing * thingplace) option
val thinginfo : name -> (thing * thingplace) option (* including invisible provisos *)
   
val clearthings : unit -> unit
val thingnames : unit -> name list
val thingstodo : unit -> bool

val clearrelationpats : unit -> unit
val addstructurerule : structurerule -> name -> bool
val wehavestructurerule : structurerule -> string list option -> (name -> bool) -> bool

val uniqueCut : unit -> name option

val isRelation : term -> bool

val thingdebug      : bool ref
val thingdebugheavy : bool ref

exception Fresh_ of string list 
exception CompileThing_ of string list
