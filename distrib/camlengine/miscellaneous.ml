(*
    $Id$

    Copyright (C) 2003-8 Richard Bornat & Bernard Sufrin
     
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

open Sml
   
let rec iter f (l, h) =
  for i = l to h do f i done

exception AtoI_

let atoi s = try Pervasives.int_of_string s with Failure _ -> raise AtoI_
let rec sum ns = List.fold_left (+) 0 ns

let rec curry2 f a b = f (a, b)
let rec uncurry2 f (a, b) = f a b

let rec curry3 f a b c = f (a, b, c)
let rec uncurry3 f (a, b, c) = f a b c

let swapargs f a b = f b a

let rec string_of_ref f {contents = a} = ("ref(" ^ f a) ^ ")"
let rec earlierpair lta ltb (a, b) (a', b') =
  lta a a' || not (lta a' a) && ltb b b' (* this is trying not to use equality ... *)

exception Catastrophe_ of string list
exception ParseError_ of string list
exception Tacastrophe_ of string list

let utf8BOM = "\xef\xbb\xbf"

let errstream : out_channel ref = ref stderr

let reporteropen = ref false

let rec create_reportfile s =
  if !reporteropen then close_out !errstream;
  errstream := open_out s;
  output_string !errstream utf8BOM;
  reporteropen := true
  
let rec close_reportfile () =
  if !reporteropen then close_out !errstream;
  errstream := stderr;
  reporteropen := false
  
let rec consolereport strings =
  let e = !errstream in
  List.iter (output_string e) strings; output_string e "\n"; flush e

let rec consolequery (message, yes, no, def) =
  List.iter (output_string stdout) message;
  output_string stdout "  ";
  let rec q () =
    List.iter (output_string stdout) [yes; "(y)/"; no; "(n)? "];
    flush stdout;
    match input_char stdin with
      'Y' -> true
    | 'y' -> true
    | 'N' -> false
    | 'n' -> false
    | _ -> s ()
  and s () =
    match input_char stdin with
      '\n' -> output_string stdout "Pardon? "; q ()
    | _ -> s ()
  in
  q ()
  
exception Error_
let rec error strings = consolereport strings; raise Error_

(* ********************************* settings variables ********************************* *)

let applyconjectures = ref "none" (* whether to allow application of conjectures and derived rules -- 
										  permitted values "all", "none", "rules", "theorems" 
								   *)

(* whether to add context automatically to rule definitions *)
let autoAdditiveLeft  = ref false
let autoAdditiveRight = ref false

let autoselect = ref true    (* whether to highlight 'next goal' when printing proofs *)
    
let givenMenuTactic = ref "" (* tactic to use when the interface says applygiven *)
 
let foldassumptionlines = ref false (* whether to fold long lines in boxdraw *)
let foldformulae = ref false (* whether to fold long formulae in boxdraw *)
let foldsequents = ref false (* whether to fold sequents in treedraw *)
  
let lemmacount = ref 0 (* number of lemmas during THIS proof *)

let multihypsel = ref false (* can select more than one hypothesis, if true *)

let resolvepossible = ref false

let screenpositiondebug = ref true
let seektipselection = ref true (* whether to look for a tip to work on in boxdraw *)
let selectiondebug = ref true
  
let textselectionmode = ref "subformula"
let truncatereasons = ref false (* whether to shorten reasons in boxdraw *)

let tryresolution = ref true

