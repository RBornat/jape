(* $Id$ *)

module type Optionfuns =
  sig
    (* infix andthen ortry
       infixr andthenr ortryr
     *)
    val anyway : ('a -> 'a option) -> 'a -> 'a
    val failpt : ('a -> 'a option) -> 'a -> 'a
    val somef : ('a -> 'a option) -> 'a -> 'a option
    val unSOME : 'a option -> 'a
    exception UnSOME_
    val try__ : ('a -> 'b) -> 'a option -> 'b option
    val opt2bool : 'a option -> bool
    val andthen : ('a -> 'b option) * ('b -> 'c option) -> 'a -> 'c option
    val andthenr : 'a option * ('a -> 'b option) -> 'b option
    val findfirst : ('a -> 'b option) -> 'a list -> 'b option
    val findbest :
      ('a -> 'b option) -> ('b -> 'b -> 'b) -> 'a list -> 'b option
    val optioncompose : ('b -> 'c) * ('a -> 'b option) -> 'a -> 'c option
    val optionmap : ('a -> 'b option) -> 'a list -> 'b list option
    val optionfilter : ('a -> 'b option) -> 'a list -> 'b list
    val optionfold : ('a * 'b -> 'b option) -> 'a list -> 'b -> 'b option
    val ortry : ('a -> 'b option) * ('a -> 'b option) -> 'a -> 'b option
    val ortryr : 'a option * (unit -> 'a option) -> 'a option
    val stripoption : 'a option option -> 'a option
    val optordefault : 'a option * 'a -> 'a
    val optionstring : ('a -> string) -> 'a option -> string
    val catelim_optionstring :
      ('a -> string list -> string list) -> 'a option -> string list ->
        string list
    (* save space when rewriting structures *)
    val option_rewrite2 :
      ('a -> 'a option) -> ('b -> 'b option) -> 'a * 'b -> ('a * 'b) option
    val option_rewrite3 :
      ('a -> 'a option) -> ('b -> 'b option) -> ('c -> 'c option) ->
        'a * 'b * 'c -> ('a * 'b * 'c) option
    val option_rewritelist : ('a -> 'a option) -> 'a list -> 'a list option
  end
 (*$Id$ *)

module Optionfuns : Optionfuns =
  struct
    exception UnSOME_
    let rec unSOME =
      function
        Some x -> x
      | None -> raise UnSOME_
    let rec try__ a1 a2 =
      match a1, a2 with
        f, Some x -> Some (f x)
      | f, None -> None
    let rec somef f x =
      match f x with
        None -> Some x
      | v -> v
    let rec anyway f x =
      match f x with
        Some v -> v
      | None -> x
    let rec failpt f x =
      match f x with
        Some y -> failpt f y
      | None -> x
    let rec opt2bool =
      function
        Some _ -> true
      | None -> false
    
    let rec andthen (f, g) x =
      match f x with
        Some y -> g y
      | None -> None
    
    let rec andthenr =
      function
        None, g -> None
      | Some v, g -> g v
    
    let rec ortry (f, g) x =
      match f x with
        None -> g x
      | v -> v
    
    let rec ortryr =
      function
        None, g -> g ()
      | v, g -> v
    let rec optioncompose (f, g) x =
      match g x with
        Some y -> Some (f y)
      | None -> None
    let rec optionmap a1 a2 =
      match a1, a2 with
        p, [] -> Some []
      | p, x :: xs ->
          andthenr
            (p x,
             (fun x -> andthenr (optionmap p xs, (fun xs -> Some (x :: xs)))))
    let rec optionfilter a1 a2 =
      match a1, a2 with
        f, [] -> []
      | f, x :: xs ->
          match f x with
            Some x' -> x' :: optionfilter f xs
          | None -> optionfilter f xs
    let rec optionfold a1 a2 a3 =
      match a1, a2, a3 with
        f, [], z -> Some z
      | f, x :: xs, z -> andthen (optionfold f xs, (fun xs' -> f (x, xs'))) z
    let rec findfirst a1 a2 =
      match a1, a2 with
        f, [] -> None
      | f, x :: xs ->
          match f x with
            None -> findfirst f xs
          | t -> t
    let rec findbest a1 a2 a3 =
      match a1, a2, a3 with
        f, best, [] -> None
      | f, best, x :: xs ->
          match f x with
            None -> findbest f best xs
          | Some y1 ->
              match findbest f best xs with
                None -> Some y1
              | Some y2 -> Some (best y1 y2)
    let rec stripoption =
      function
        None -> None
      | Some x -> x
    let rec optordefault =
      function
        Some v, _ -> v
      | None, v -> v
    let rec catelim_optionstring catelim_astring aopt ss =
      match aopt with
        Some a -> "Some (" :: catelim_astring a (")" :: ss)
      | None -> "None" :: ss
    let rec optionstring astring aopt =
      implode (catelim_optionstring (fun a ss -> astring a :: ss) aopt [])
    (* save space when rewriting structures *)
    let rec option_rewrite2 fa fb (a, b) =
      match fa a, fb b with
        Some a, Some b -> Some (a, b)
      | Some a, None -> Some (a, b)
      | None, Some b -> Some (a, b)
      | None, None -> None
    (* there must be a better way ... *)
    let rec option_rewrite3 fa fb fc (a, b, c) =
      match fa a, fb b, fc c with
        Some a, Some b, Some c -> Some (a, b, c)
      | Some a, Some b, None -> Some (a, b, c)
      | Some a, None, Some c -> Some (a, b, c)
      | Some a, None, None -> Some (a, b, c)
      | None, Some b, Some c -> Some (a, b, c)
      | None, Some b, None -> Some (a, b, c)
      | None, None, Some c -> Some (a, b, c)
      | None, None, None -> None
    let rec option_rewritelist a1 a2 =
      match a1, a2 with
        f, [] -> None
      | f, ( :: ) pair ->
          andthen
            (option_rewrite2 f (option_rewritelist f),
             (fun ooo -> Some ((fun (x, y) -> x :: y) ooo)))
            pair
  end
