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

open Box
open Prooftree.Tree.Fmttree
open Proofstate
open Hit
open Cxttype
open Displaystate
open Termtype
open Sequent

type command =
    TextCommand of string list
  | HitCommand of (prooftree * path hit * path sel)

val string_of_command : command -> string
val terminateGUI : unit -> unit
val reportGUIdead : string list -> unit
val setdisplaystyle : string -> unit
val getdisplaystyle : unit -> string
val showProof : displaystate -> path option -> path option -> cxt -> prooftree 
             -> bool -> displaystate
val showFocussedProof : path option -> cxt -> prooftree -> bool -> displaystate
val refreshProof : displaystate -> unit
val displayProvisos : cxt -> unit
val displayGivens : seq list -> unit
val showallprovisos : bool ref
val getCommand : displaystate option -> command
val findSelection : displaystate -> path sel option
val findLayoutSelection : displaystate -> hitkind -> path option
val findDisproofSelections: unit -> pos list * (pos * string list) list
(* Drag n drop is moribund, as currently implemented.  Will be redone! *)
val dropsource : element option ref
val droptarget : element option ref
val setComment : string list -> unit
val showState : displaystate -> proofstate -> bool -> displaystate
val printState : Stdlib.out_channel -> proofstate -> bool -> unit
val alterTip : displaystate -> cxt -> path -> prooftree
            -> (prooftree * path) option -> (bool * path * element) * string list
            -> cxt * element * prooftree
val locateElement : displaystate -> element -> pos list