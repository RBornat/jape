(* $Id$ *)

open Listfuns
open Symbol
open Term.Funs
open Term.Store

type paraparam = Objectparam of (vid * idclass)
               | Ordinaryparam of (vid * idclass)
               | Unknownparam of (vid * idclass)
               | Abstractionparam of (vid * idclass)

(* ABSTRACTION P *) (*huh?*)
 
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

