(* $Id$ *)

(* don't import Nametype.  At present only paragraph and tactic are entitled *)

(* but in the blasted OCaml system, I can't hide it so easily. *)

(* module type Nametype = 
  sig 
    type name = Name of string 
  end
*)

module type T = (* all the stuff that's fit to print *)
  sig
    type name = Name of string 
     and term
    val namestring : name -> string
    val parseablenamestring : name -> string
    val nameorder : name -> name -> bool
    val term2name : term -> name option
    val namefrom : string -> name
  end
  
(* $Id$ *)
module M : (* sig include Nametype include Name end *) T with type term = Term.Funs.term 
=
  struct
    open Miscellaneous
    open Stringfuns.M
    open Symbol 
    open Symboltype 
    open Term.Funs
    open Term.Type 
    open Termparse.M
    
    type term = Term.Funs.term
    
    (* it is high time we had a datatype of names of things *)
    
    type name = Name of string
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
          ParseError_ _ -> enQuote (String.escaped s)
    let rec term2name t =
      match t with
        Id (_, v, _) -> Some (Name (string_of_vid v))
      | Unknown (_, v, _) -> Some (Name (metachar ^ string_of_vid v))
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
  end
