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

module M (AAA : sig type idclass and vid end) : T =
  struct
    open AAA
    type idclass = AAA.idclass and vid = AAA.vid
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
