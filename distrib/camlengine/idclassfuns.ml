(* $Id$ *)

open Symboltype 
open Symbol 
open Idclass
open Miscellaneous
open Listfuns

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
    SHYID "FORMULA" -> let _ = scansymb () in FormulaClass
  | SHYID "VARIABLE" -> let _ = scansymb () in VariableClass
  | SHYID "CONSTANT" -> let _ = scansymb () in ConstantClass
  | SHYID "NUMBER" -> let _ = scansymb () in NumberClass
  | SHYID "STRING" -> let _ = scansymb () in StringClass
  | SHYID "BAG" ->
      let _ = scansymb () in
      BagClass
        (if canstartidclass (currsymb ()) then parseidclass "BAG"
         else FormulaClass)
  | SHYID "LIST" ->
      let _ = scansymb () in
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
