(* $Id$ *)

open Term.Funs
open Context.Cxt

val unifyterms : term * term -> cxt -> cxt option
val unifytermsandcheckprovisos : term * term -> cxt -> cxt option
val unifyvarious : term * term -> cxt -> cxt list
val dropunify : element * element list -> cxt -> cxt option
val simplifydeferred : cxt -> cxt option
val matchedtarget : cxt -> cxt -> vid list -> bool
val unifydebug : bool ref
