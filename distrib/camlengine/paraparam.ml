(* $Id$ *)

module type Paraparam =
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

module
  Paraparam
  (AAA :
    sig
      type vid and idclass and term
      val catelim2stringfn :
        ('a -> string list -> string list) -> 'a -> string
      val Id : vid * idclass -> term
      val metachar : string
      val Unknown : vid * idclass -> term
      
    end)
  :
  Paraparam =
  struct
    open AAA
    type vid = vid and idclass = idclass and term = term
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
        Objectparam vc -> Id vc
      | Ordinaryparam vc -> Id vc
      | Unknownparam vc -> Unknown vc
      | Abstractionparam vc -> Id vc
  end

