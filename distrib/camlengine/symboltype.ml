(* $Id$ *)

module type T =
  sig
    type vid and idclass
    type associativity =
      LeftAssoc | RightAssoc | AssocAssoc | TupleAssoc | CommAssocAssoc
    type symbol =
        ID of (vid * idclass option)
      | UNKNOWN of (vid * idclass option)
      | NUM of string
      | STRING of string
      | BRA of string
      | SEP of string
      | KET of string
      | SUBSTBRA
      | SUBSTKET
      | SUBSTSEP
      | EOF
      | PREFIX of (int * string)
      | POSTFIX of (int * string)
      | INFIX of (int * associativity * string)
      | INFIXC of (int * associativity * string)
      | LEFTFIX of (int * string)
      | MIDFIX of (int * string)
      | RIGHTFIX of (int * string)
      | STILE of string
      | SHYID of string
  end
(* $Id$ *)

module M : T with type vid = string 
              and type idclass = Idclass.M.idclass
=
  struct

    type idclass = Idclass.M.idclass and vid = string
    
    type associativity =
      LeftAssoc | RightAssoc | AssocAssoc | TupleAssoc | CommAssocAssoc
    type symbol =
        ID of (vid * idclass option)
      | UNKNOWN of (vid * idclass option)
      | NUM of string
      | STRING of string
      | BRA of string
      | SEP of string
      | KET of string
      | SUBSTBRA
      | SUBSTKET
      | SUBSTSEP
      | EOF
      | PREFIX of (int * string)
      | POSTFIX of (int * string)
      | INFIX of (int * associativity * string)
      | INFIXC of (int * associativity * string)
      | LEFTFIX of (int * string)
      | MIDFIX of (int * string)
      | RIGHTFIX of (int * string)
      | STILE of string
      | SHYID of string
  end
