(*
    Copyright (C) 2003-17 Richard Bornat & Bernard Sufrin
     
        richard@bornat.me.uk
        sufrin@comlab.ox.ac.uk

    This file is part of the jape proof engine, which is part of jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).

*)

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
            "expected "; prev; " -- found "; string_of_symbol s])

(* this is deliberately not string_of_idclass -- because of the SHYID implications *)
let rec unparseidclass =
  function
    FormulaClass -> "FORMULA"
  | VariableClass -> "VARIABLE"
  | ConstantClass -> "CONSTANT"
  | NumberClass -> "NUMBER"
  | StringClass -> "STRING"
  | BagClass c -> "BAG " ^ unparseidclass c
  | ListClass c -> "LIST " ^ unparseidclass c
  | c -> string_of_idclass c

(* this is the inverse of string_of_idclass (and it is kind to people who use unparseidclass) *)
let rec idclass_of_string prev =
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
            "expected "; prev; " -- found "; string_of_symbol s])
