(* $Id$ *)

open Term.Type
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

val empty : japeenv
val ( ++ ) : japeenv * japeenv -> japeenv
val ( |-> ) : name * term -> japeenv
val ( ||-> ) : name * japevar -> japeenv
val at : japeenv * name -> term option
val set : japeenv * name * term -> unit
val checkrange : japeenv -> name -> string list -> unit

exception OutOfRange_ of string 
exception NotJapeVar_ 
exception ReadOnly_

