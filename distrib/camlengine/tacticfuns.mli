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

open Prooftree.Tree.Fmttree
open Proofstate
open Tactictype
open Japeenv
open Displaystate
open Name
open Termtype
open Hit

val forceUnify  : term list -> proofstate -> proofstate option
val doDropUnify : element -> element list -> proofstate -> proofstate option
val autoStep    : bool -> name list -> proofstate -> proofstate option
val selections  : (path * 
                   (element * side option) option * 
                   element list *
                   (path * (element * side option) * string list) list *
                   (path * element * string list) list * 
                   string list)
                  option ref
val applyLiteralTactic :
  displaystate option -> japeenv -> string -> proofstate -> proofstate option
val applyTactic :
  displaystate option -> japeenv -> tactic -> proofstate -> proofstate option
val autoTactics :
  displaystate option -> japeenv -> (bool * tactic) list -> proofstate -> proofstate
val interruptTactic : unit -> unit
val resetcaches : unit -> unit
val explain : string -> string list

val _FINDdebug : bool ref
val _FOLDdebug : bool ref

val proving          : name ref
val tacticresult     : string ref
val tactictracing    : bool ref
val timestotry       : int ref
val tryresolution    : bool ref
val applyautotactics : bool ref
