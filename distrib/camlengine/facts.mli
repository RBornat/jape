(* $Id$ *)

open Proviso
open Context.Type
open Term.Type
open Answer

type facts

val expandfacts : facts -> proviso list -> facts
val exterioreqvarsq : facts -> term -> term -> answer
val facts : visproviso list -> cxt -> facts
val knownNOTIN : facts -> term * term -> bool
val knownproofvar : facts -> term -> bool
val substeqvarsq : facts -> term -> term -> answer
val unifyeqtermsq : facts -> term -> term -> answer

val factsdebug : bool ref

val factsstring : facts -> string

