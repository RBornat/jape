(* $Id$ *)

module type T =
  sig
    type prestring =
      BQuote1 of string | BQuote2 of string list | BQuote3 of prestring list
    val pre_List : ('a -> prestring) -> 'a list -> prestring
    val pre_Set : ('a -> prestring) -> 'a list -> prestring
    val pre_Tuple : ('a -> prestring) -> 'a list -> prestring
    val pre__comma : prestring
    val pre__nil : prestring
    val pre__space : prestring
    val pre_app : (string -> unit) -> prestring -> unit
    val pre_array : ('a -> prestring) -> 'a array -> prestring
    val pre_bool : bool -> prestring
    val pre_implode : prestring -> string
    val pre_int : int -> prestring
    val pre_list : ('a -> prestring) -> 'a list -> prestring
    val pre_option : ('a -> prestring) -> 'a option -> prestring
    val pre_real : float -> prestring
    val pre_string : string -> prestring
    val pre_unit : unit -> prestring
    val pre_vector : ('a -> prestring) -> 'a array -> prestring
  end
(* $Id$ *)

(*
        A prestring structure makes it simpler to construct
    reasonably-efficient but perspicacious pretty-printers

    It can be used with pretty-printers produced
    by the following mldata command:

    mldata -pre Pre -prim Pre -string 'BQuote1' -hom 'BQuote3' $*
*)

module M : T =
  struct
    open Sml.M
    
    type prestring =
      BQuote1 of string | BQuote2 of string list | BQuote3 of prestring list
    let pre_string s = BQuote1 s
    let pre__comma = BQuote1 ", "
    let pre__nil = BQuote3 []
    let pre__space = BQuote1 " "
    (*  Primitive preprinters *)
    let rec pre_int (i : int) = BQuote1 (string_of_int i)
    let rec pre_real (r : float) = BQuote1 (string_of_float r)
    let rec pre_unit () = BQuote1 "()"
    let rec pre_bool (b : bool) = BQuote1 (string_of_bool b)
    (*  Built-in constructors *)
    let rec pre_option a1 a2 =
      match a1, a2 with
        f, None -> BQuote1 "None"
      | f, Some x -> BQuote3 [BQuote1 "Some("; f x; BQuote1 ")"]
    let rec pre_array f a =
      let s = Array.length a in
      let rec p n =
        if n = s then []
        else
          f (Array.get a n) ::
            (if n + 1 = s then pre__nil else pre__space) :: p (n + 1)
      in
      BQuote3 (p 0)
    let rec pre_vector f a =
      let s = Array.length a in
      let rec p n =
        if n = s then []
        else
          BQuote3 [pre_int n; BQuote1 ":"; f (Array.get a n)] ::
            (if n + 1 = s then pre__nil else pre__comma) :: p (n + 1)
      in
      BQuote3 [BQuote1 "["; BQuote3 (p 0); BQuote1 "]"]
    exception Matchinpre_Comma
    (* spurious *)
    let rec pre_Comma a1 a2 =
      match a1, a2 with
        f, [] -> BQuote3 []
      | f, [x] -> BQuote3 [f x]
      | f, x :: xs ->
          match pre_Comma f xs with
            BQuote3 ps -> BQuote3 (f x :: pre__comma :: ps)
          | _ -> raise Matchinpre_Comma
    let rec pre_Tuple f xs =
      BQuote3 [BQuote1 "("; pre_Comma f xs; BQuote1 ")"]
    let rec pre_Set f xs = BQuote3 [BQuote1 "{"; pre_Comma f xs; BQuote1 "}"]
    let rec pre_List f xs =
      BQuote3 [BQuote1 "["; pre_Comma (f) (xs); BQuote1 "]"]
    let rec pre_list f xs =
      BQuote3 [BQuote1 "["; pre_Comma (f) (xs); BQuote1 "]"]
    
    let rec pre_implode p =
      let rec _I (p,ss) =
        match p with
          BQuote1 s -> s :: ss
        | BQuote2 sl -> sl @ ss
        | BQuote3 ps -> nj_fold _I ps ss
      in
      implode (_I (p,[]))
    
    let rec pre_app a1 a2 =
      match a1, a2 with
        p, BQuote1 s -> p s
      | p, BQuote2 ss -> List.iter p ss
      | p, BQuote3 ps -> List.iter (pre_app p) ps
  end
