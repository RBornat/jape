(* $Id$ *)

module type Idclassfuns =
  sig
    type idclass and symbol
    val canstartidclass : symbol -> bool
    val canstartCollectionidclass : symbol -> bool
    val parseidclass : string -> idclass
    val unparseidclass : idclass -> string
  end
(* $Id$ *)

module
  Idclassfuns
  (AAA :
    sig
      module symboltype : Symboltype
      module symbol : Symbol
      module idclass : Idclass
      val member : 'a * 'a list -> bool
      exception ParseError_ of string list
      
    end)
  :
  Idclassfuns =
  struct
    open AAA
    open symboltype open symbol open idclass
    
    (* in future we shall have lots of kinds of Bags and Lists; for the moment 
     * it would be burdensome to force the world to say BAG FORMULA, so we don't.
     * RB 2/10/96
     *)

    let rec canstartidclass sy =
      member
        (sy,
         [SHYID "FORMULA"; SHYID "VARIABLE"; SHYID "CONSTANT"; SHYID "NUMBER";
          SHYID "STRING"; SHYID "BAG"; SHYID "LIST"])
    let rec canstartCollectionidclass sy =
      member (sy, [SHYID "BAG"; SHYID "LIST"])
    let rec parseidclass prev =
      match currsymb () with
        SHYID "FORMULA" -> scansymb (); FormulaClass
      | SHYID "VARIABLE" -> scansymb (); VariableClass
      | SHYID "CONSTANT" -> scansymb (); ConstantClass
      | SHYID "NUMBER" -> scansymb (); NumberClass
      | SHYID "STRING" -> scansymb (); StringClass
      | SHYID "BAG" ->
          scansymb ();
          BagClass
            (if canstartidclass (currsymb ()) then parseidclass "BAG"
             else FormulaClass)
      | SHYID "LIST" ->
          scansymb ();
          ListClass
            (if canstartidclass (currsymb ()) then parseidclass "List"
             else FormulaClass)
      | s ->
          raise
            (ParseError_
               ["BAG, LIST, FORMULA, VARIABLE, CONSTANT, NUMBER or STRING ";
                "expected "; prev; " -- found "; symbolstring s])
    let rec unparseidclass =
      function
        FormulaClass -> "FORMULA"
      | VariableClass -> "VARIABLE"
      | ConstantClass -> "CONSTANT"
      | NumberClass -> "NUMBER"
      | StringClass -> "STRING"
      | BagClass c -> "BAG " ^ unparseidclass c
      | ListClass c -> "LIST " ^ unparseidclass c
      | c -> idclassstring c
  end
