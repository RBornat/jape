(*
	$Id$

    Copyright (C) 2003-4 Richard Bornat & Bernard Sufrin
     
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

(* Variables now have names.  Their values are still strings. *)

type japevar and japeenv

val japevar :
  string list -> string -> (string -> unit) * (unit -> string) -> japevar
val japerefvar : string list -> string -> string ref -> japevar
val unboundedjapevar :
  string -> (string -> unit) * (unit -> string) -> japevar
val unboundedjaperefvar : string -> string ref -> japevar
val booljapevar : bool -> (bool -> unit) * (unit -> bool) -> japevar
val booljaperefvar : bool -> bool ref -> japevar
val intjapevar : int -> (int -> unit) * (unit -> int) -> japevar
val intjaperefvar : int -> int ref -> japevar
val resetvar : japevar -> unit
val guardedjapevar : (unit -> bool) -> japevar -> japevar
(* guardedjapevars can't be set unless the guard says true.
 * example: vars which can be set until there is something in the thing store 
 *)

val ( ++   ) : japeenv -> japeenv -> japeenv
val ( |->  ) : name -> term -> japeenv
val ( ||-> ) : name -> japevar -> japeenv

val (<@>)      : japeenv -> name -> term option
val checkrange : japeenv -> name -> string list -> unit
val empty      : japeenv
val set        : japeenv * name * term -> unit

exception OutOfRange_ of string 
exception NotJapeVar_ 
exception ReadOnly_

val string_of_japeenv : japeenv -> string