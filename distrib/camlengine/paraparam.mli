open Term.Funs 

type paraparam = Objectparam of (vid * idclass)
               | Ordinaryparam of (vid * idclass)
               | Unknownparam of (vid * idclass)
               | Abstractionparam of (vid * idclass)

(* ABSTRACTION P *) (* whatever that means! RB *)

val paramidbits : paraparam -> vid * idclass
val paramvar : paraparam -> term

val catelim_paraparamstring : paraparam -> string list -> string list
val paraparamstring : paraparam -> string
