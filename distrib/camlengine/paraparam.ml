(* $Id$ *)

module type T =
  sig
    type vid and idclass and term
    type paraparam =
        Objectparam of (vid * idclass)
      | Ordinaryparam of (vid * idclass)
      | Unknownparam of (vid * idclass)
      | Abstractionparam of (vid * idclass)
    (* ABSTRACTION P *)
    val catelim_paraparamstring : paraparam -> string list -> string list
    val paraparamstring : paraparam -> string
    val paramidbits : paraparam -> vid * idclass
    val paramvar : paraparam -> term
  end
(* $Id$ *)

module M : T with type vid = Term.Funs.vid 
              and type idclass = Term.Funs.idclass 
              and type term = Term.Funs.term 
=
  struct
    open Listfuns.M
    open Symbol.Funs
    open Term.Funs
    open Term.Store
    
    type vid = Term.Funs.vid 
     and idclass = Term.Funs.idclass 
     and term = Term.Funs.term
    
    type paraparam =
        Objectparam of (vid * idclass)
      | Ordinaryparam of (vid * idclass)
      | Unknownparam of (vid * idclass)
      | Abstractionparam of (vid * idclass)
    (* ABSTRACTION P *)
     
    let rec catelim_paraparamstring p tail =
      match p with
        Objectparam (v, _) -> "OBJECT " :: string_of_vid v :: tail
      | Ordinaryparam (v, _) -> string_of_vid v :: tail
      | Unknownparam (v, _) -> metachar :: string_of_vid v :: tail
      | Abstractionparam (v, _) -> "ABSTRACTION " :: string_of_vid v :: tail
    let paraparamstring = catelim2stringfn catelim_paraparamstring
    let rec paramidbits p =
      match p with
        Objectparam vc -> vc
      | Ordinaryparam vc -> vc
      | Unknownparam vc -> vc
      | Abstractionparam vc -> vc
    let rec paramvar p =
      match p with
        Objectparam vc -> registerId vc
      | Ordinaryparam vc -> registerId vc
      | Unknownparam vc -> registerUnknown vc
      | Abstractionparam vc -> registerId vc
  end

