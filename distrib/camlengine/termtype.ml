type vid = string (* but nobody else knows *) 
 and idclass = Idclass.idclass

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

let rec resnum2int =
  function
	Resnum n -> n
  | ResUnknown n -> n
  | Nonum -> 0

(* We keep the user's bracket structure, so every time we match/unify/compare
   two terms, we must debracket them
 *)
let rec debracket =
  function
	Fixapp (_, ["("; ")"], [t]) -> debracket t
  | t -> t
let rec bracketed =
  function
	Fixapp (_, ["("; ")"], [t]) -> true
  | t -> false

let vid_of_string s = s
let string_of_vid v = v
