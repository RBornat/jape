(* $Id$ *)

open Nametype

open Miscellaneous
open Stringfuns
open Symbol 
open Symboltype 
open Term.Funs
open Term.Type 
open Termparse

type name = Nametype.name
 and term = Term.Funs.term

let namestring (Name s) = s
let nameorder (Name s1) (Name s2) = s1 < s2

(* this thing ain't cateliminated.  Can you see a way to do it? 
 * I can't: not without a cateliminated explode, and even then ...
 *)
let rec parseablenamestring =
  fun (Name s) ->
    let rec parsename sy =
      match currsymb () with
        ID _ -> scansymb ()
      | STRING _ -> raise (Catastrophe_ ["double quoting in Name "; s])
      | _ -> raise (ParseError_ [])
    in
    try let _ = tryparse parsename s in s with
      ParseError_ _ -> enQuote s 
        (* String.escaped would seem to be useful here, but it calls isprint 
           (see the Unix manual) and that, on MacOS X if nowhere else, objects 
           to most of the funny characters in Konstanz font (for example).
           So no escaping for the time being, unless and until we can tell the 
           system what font we want isprint to work in.
         *)

let rec term2name t =
  match t with
    Id (_, v, _)          -> Some (Name (string_of_vid v))
  | Unknown (_, v, _)     -> Some (Name (metachar ^ string_of_vid v))
  | Literal (_, Number s) -> Some (Name (string_of_int s))
  | Literal (_, String s) -> Some (Name s)
  | _ -> None

(* this is better than unQuote, because it parses the string *)
let rec namefrom s =
  try
    match tryparse parseTerm s with
      Literal (_, String s) -> Name s
    | _ ->(* without the quotes *)
       raise (ParseError_ [])
  with
    ParseError_ _ -> Name s
