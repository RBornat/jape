(* $Id$ *)

(* don't import Nametype.  At present only paragraph and tactic are entitled *)
module type Nametype = sig type name = Name of string end

module type Name =
  sig
    type name
    type term
    val namestring : name -> string
    val parseablenamestring : name -> string
    val nameorder : name * name -> bool
    val term2name : term -> name option
    val namefrom : string -> name
  end

(* $Id$ *)
module
  Name
  (AAA :
    sig
      module symboltype : Symboltype
      module symbol : Symbol
      module term : Termtype
      (* sanctioned in name. RB *)
      module termparse : Termparse
      val enQuote : string -> string
      exception Catastrophe_ of string list
      exception ParseError_ of string list
      
    end)
  :
  sig include Nametype include Name end =
  struct
    open AAA
    open symboltype open symbol open term open termparse
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
        try tryparse parsename s; s with
          ParseError_ _ -> enQuote (implode (List.map unescapechar (explode s)))
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
