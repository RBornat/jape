(* $Id$ *)

(* don't import Nametype.  At present only paragraph and tactic are entitled *)

(* but in the blasted OCaml system, I can't hide it so easily.  I'll do it the slow way *)

module type Nametype = 
  sig 
    type name = Name of string 
    type term
    val namestring : name -> string
    val parseablenamestring : name -> string
    val nameorder : name * name -> bool
    val term2name : term -> name option
    val namefrom : string -> name
  end

module type T = (* all the stuff that's fit to print *)
  sig
    type name and term
    val namestring : name -> string
    val parseablenamestring : name -> string
    val nameorder : name * name -> bool
    val term2name : term -> name option
    val namefrom : string -> name
  end
  
(* $Id$ *)
module M
  (AAA :
    sig
      module Symboltype : Symboltype.T
      module Symbol : Symbol.T
             with type symbol = Symboltype.symbol
      module Term : (*Termtype*)Term.T
             with type vid = string
      (* sanctioned in name. RB *)
      module Termparse : Termparse.T
             with type term = Term.term
      
      val enQuote : string -> string
      
      exception Catastrophe_ of string list
      exception ParseError_ of string list
    end)
  : (* sig include Nametype include Name end *) Nametype =
  struct
    open AAA
    open Symboltype 
    open Symbol 
    open Term 
    open Termparse
    
    type term = Term.term
    
    (* it is high time we had a datatype of names of things *)
    
    type name = Name of string
    let rec namestring = fun (Name s) -> s
    let rec nameorder = fun (Name s1, Name s2) -> s1 < s2
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
        Id (_, s, _) -> Some (Name s)
      | Unknown (_, s, _) -> Some (Name (metachar ^ s))
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
