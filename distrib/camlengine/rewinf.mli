(* $Id$ *)

open Term.Funs

type rewinf

val mkrewinf : term list * vid list * int list * int option -> rewinf
val nullrewinf : rewinf
val rew2rawinf : rewinf -> term list * vid list * int list * int option
val rewinf_addbadres : rewinf -> int list -> rewinf
val rewinf_adduVIDs : rewinf -> vid list -> rewinf
val rewinf_addvars : rewinf -> term list -> rewinf
val rewinf_badres : rewinf -> int list
val rewinf_merge : rewinf * rewinf -> rewinf
val rewinf_psig : rewinf -> int option
val rewinf_setbadres : rewinf -> int list -> rewinf
val rewinf_setpsig : rewinf -> int option -> rewinf
val rewinf_setuVIDs : rewinf -> vid list -> rewinf
val rewinf_setvars : rewinf -> term list -> rewinf
val rewinf_uVIDs : rewinf -> vid list
val rewinf_vars : rewinf -> term list

val rewinfstring : rewinf -> string


