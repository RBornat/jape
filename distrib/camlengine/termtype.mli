open Idclass

type vid

(* terms now contain hash information. RB 26/i/00 *)
(* It's become an option so we don't cache terms which contain unknowns. RB 27/i/00 *)
type term =
	Id of (int option * vid * idclass)
  | Unknown of (int option * vid * idclass)
  | App of (int option * term * term)
  | Tup of (int option * string * term list)
  | Literal of (int option * litcon)
  | Fixapp of (int option * string list * term list)
  | Subst of (int option * bool * term * (term * term) list)
  | Binding of
	  (int option * (term list * term list * term list) *
		 (term * (int * int)) list * term)
  | Collection of (int option * idclass * element list)
and litcon = Number of int | String of string
and element =
	Segvar of (int option * term list * term)
  | Element of (int option * resnum * term)
and resnum = Nonum | Resnum of int | ResUnknown of int

val bracketed : term -> bool
val debracket : term -> term
val resnum2int : resnum -> int

val string_of_vid : vid -> string
val vid_of_string : string -> vid
