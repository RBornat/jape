(* $Id$ *)

open Sml

type prestring =
  Prestr of string | Prestrs of string list | Prepres of prestring list

let pre_string s = Prestr s
let pre__comma = Prestr ", "
let pre__nil = Prepres []
let pre__space = Prestr " "

(*  Primitive preprinters *)

let rec pre_int (i : int) = Prestr (string_of_int i)
let rec pre_real (r : float) = Prestr (string_of_float r)
let rec pre_unit () = Prestr "()"
let rec pre_bool (b : bool) = Prestr (string_of_bool b)

(*  Built-in constructors *)
let rec pre_option a1 a2 =
  match a1, a2 with
	f, None -> Prestr "None"
  | f, Some x -> Prepres [Prestr "Some("; f x; Prestr ")"]
let rec pre_array f a =
  let s = Array.length a in
  let rec p n =
	if n = s then []
	else
	  f (Array.get a n) ::
		(if n + 1 = s then pre__nil else pre__space) :: p (n + 1)
  in
  Prepres (p 0)
let rec pre_vector f a =
  let s = Array.length a in
  let rec p n =
	if n = s then []
	else
	  Prepres [pre_int n; Prestr ":"; f (Array.get a n)] ::
		(if n + 1 = s then pre__nil else pre__comma) :: p (n + 1)
  in
  Prepres [Prestr "["; Prepres (p 0); Prestr "]"]
exception Matchinpre_Comma
(* spurious *)
let rec pre_Comma a1 a2 =
  match a1, a2 with
	f, [] -> Prepres []
  | f, [x] -> Prepres [f x]
  | f, x :: xs ->
	  match pre_Comma f xs with
		Prepres ps -> Prepres (f x :: pre__comma :: ps)
	  | _ -> raise Matchinpre_Comma
let rec pre_Tuple f xs =
  Prepres [Prestr "("; pre_Comma f xs; Prestr ")"]
let rec pre_Set f xs = Prepres [Prestr "{"; pre_Comma f xs; Prestr "}"]
let rec pre_List f xs =
  Prepres [Prestr "["; pre_Comma (f) (xs); Prestr "]"]
let rec pre_list f xs =
  Prepres [Prestr "["; pre_Comma (f) (xs); Prestr "]"]

let rec pre_implode p =
  let rec _I (p,ss) =
	match p with
	  Prestr s -> s :: ss
	| Prestrs sl -> sl @ ss
	| Prepres ps -> nj_fold _I ps ss
  in
  implode (_I (p,[]))

let rec pre_app a1 a2 =
  match a1, a2 with
	p, Prestr s -> p s
  | p, Prestrs ss -> List.iter p ss
  | p, Prepres ps -> List.iter (pre_app p) ps
