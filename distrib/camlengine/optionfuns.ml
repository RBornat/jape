(*  $Id$ *)


open Sml

exception None_

let rec _The = function Some x -> x | None -> raise None_

let rec opt2bool = function Some _ -> true | None -> false

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

let rec (&~~) v g =
  match v with 
    None    -> None
  | Some v' -> g v'

let rec (|~~) v g =
  match v with
    None -> g ()
  | v    -> v

let rec (&~) f g x = f x &~~ g

let rec (|~) f g x =
  match f x with
    None -> g x
  | v    -> v

let rec optioncompose (f, g) x =
  match g x with
    Some y -> Some (f y)
  | None -> None

let rec optionmap a1 a2 =
  match a1, a2 with
    p, [] -> Some []
  | p, x :: xs -> p x &~~ (fun x -> (optionmap p xs &~~ (fun xs -> Some (x::xs))))
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
  | f, x :: xs, z -> optionfold f xs z &~~ (fun xs' -> f (x, xs'))
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

(* the next two could be composed from option_rewrite2, but that would cause churn *)
let rec option_rewrite3 fa fb fc (a, b, c) =
  match fa a, fb b, fc c with
    Some a, Some b, Some c -> Some (a, b, c)
  | Some a, Some b, None   -> Some (a, b, c)
  | Some a, None  , Some c -> Some (a, b, c)
  | Some a, None  , None   -> Some (a, b, c)
  | None  , Some b, Some c -> Some (a, b, c)
  | None  , Some b, None   -> Some (a, b, c)
  | None  , None  , Some c -> Some (a, b, c)
  | None  , None  , None   -> None
let rec option_rewritelist f xs =
  match xs with
    []    -> None
  | x::xs -> match f x, option_rewritelist f xs with
               Some x, Some xs -> Some (x::xs)
             | Some x, None    -> Some (x::xs)
             | None  , Some xs -> Some (x::xs)
             | None  , None    -> None
        
