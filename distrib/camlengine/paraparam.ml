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

module M : T with type vid = Term.M.vid 
              and type idclass = Idclass.M.idclass 
              and type term = Term.M.term 
=
  struct
    open Symbol.M
    open Listfuns.M
    open Term.M
    
    type vid = Term.M.vid and idclass = Idclass.M.idclass and term = Term.M.term
    
    type paraparam =
        Objectparam of (vid * idclass)
      | Ordinaryparam of (vid * idclass)
      | Unknownparam of (vid * idclass)
      | Abstractionparam of (vid * idclass)
    (* ABSTRACTION P *)
     
    let rec catelim_paraparamstring p tail =
      match p with
        Objectparam (v, _) -> "OBJECT " :: v :: tail
      | Ordinaryparam (v, _) -> v :: tail
      | Unknownparam (v, _) -> metachar :: v :: tail
      | Abstractionparam (v, _) -> "ABSTRACTION " :: v :: tail
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

