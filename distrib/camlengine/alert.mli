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

type alertspec =
    Alert of (string * (string * alertspec option) list * int)
  | HowToTextSelect
  | HowToFormulaSelect
  | HowToDrag

type alertseverity = Info | Warning | Error | Question

val ask : alertseverity -> string -> (string * 'a) list -> int       -> 'a
                        (* message   buttons  actions      defaultindex  result   *)

val askCancel :
  alertseverity -> string -> (string * 'a) list -> 'a      -> int        -> 'a
                (* message   buttons   actions  cancelaction  defaultindex  result   
                 *    (set defaultindex = List.length buttons to choose Cancel)
                 *)


val askDangerously : string    -> string * 'a -> string * 'a -> 'a   -> 'a
                  (* message      Do             Don't          Cancel   Result
                   *
                   * special version of askCancel, with Do as default, and
                   * the buttons in "Do/Don't" positions -- like this
                   * 
                   * ICON
                   * ICON                    message
                   * ICON 
                   * 
                   * Don't                Cancel  Do
                   *)

val askChoice : string * string list list -> int option
val defaultseverity       : 'a list -> alertseverity
val defaultseverity_alert : alertseverity
val patchalert            : string * alertspec -> unit
val resetalertpatches     : unit -> unit
val setComment            : string -> unit (* this demoted to a thing which sets a comment line *)
val showAlert             : alertseverity -> string -> unit (* this pops up a window *)

