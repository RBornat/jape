(*
	$Id$

    Copyright (C) 2003-4 Richard Bornat & Bernard Sufrin
     
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
  if l > h then () else begin f l; iter f (l + 1, h) end
let rec charpred s =
  let v = Array.make 256 false in
  iter (fun i -> Array.set v (Char.code (String.get s i)) true) (0, String.length s - 1);
  (fun c -> Array.get v (Char.code (String.get c 0))),
  (fun (c, b) -> Array.set v (Char.code (String.get c 0)) b)
let (islcletter, _) = charpred "abcdefghijklmnopqrstuvwxyz"
let (isucletter, _) = charpred "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rec isletter c = islcletter c || isucletter c
let (isdigit, _) = charpred "0123456789"
exception AtoI_
let atoi s = try Pervasives.int_of_string s with Failure _ -> raise AtoI_
let errstream : out_channel ref = ref stderr
let reporteropen = ref false
let rec create_reportfile s =
  if !reporteropen then close_out !errstream;
  errstream := open_out s;
  reporteropen := true
let rec close_reportfile () =
  if !reporteropen then close_out !errstream;
  errstream := stderr;
  reporteropen := false
let rec consolereport strings =
  let e = !errstream in
  List.iter (output_string e) strings; output_string e "\n"; flush e
let observe = consolereport
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
let rec sum ns = List.fold_left (+) 0 ns
let rec curry2 f a b = f (a, b)
let rec uncurry2 f (a, b) = f a b
let rec refstring f {contents = a} = ("ref(" ^ f a) ^ ")"
let rec earlierpair lta ltb (a, b) (a', b') =
  lta a a' || not (lta a' a) && ltb b b' (* this is trying not to use equality ... *)
  
(* whether to add context automatically to rule definitions *)
let autoAdditiveLeft  = ref false
let autoAdditiveRight = ref false
    
let lemmacount = ref 0 (* number of lemmas during THIS proof *)
let applyconjectures = ref true (* whether to allow application of conjectures *)
let applyderivedrules = ref true (* whether to allow application of derived rules *)
let autoselect = ref true    (* whether to highlight 'next goal' when printing proofs *)
let givenMenuTactic = ref "" (* tactic to use when the interface says applygiven *)
 
let foldformulae = ref false (* whether to fold long lines in boxdraw *)
let truncatereasons = ref false (* whether to shorten reasons in boxdraw *)
  
let seektipselection = ref true (* whether to look for a tip to work on in boxdraw *)
  
let textselectionmode = ref "subformula"
let screenpositiondebug = ref true

let tryresolution = ref true
let resolvepossible = ref false

(* this collection avoids low characters which might be useful *)
(* 0 is NUL; can't appear in a C string, so don't use *)
let onbra = '\001'  (* SOH *)
let onket = '\002'  (* STX *)
let offbra = '\003' (* ETX *)
let offket = '\004' (* EOT *)
let outbra = '\005' (* ENQ *)
let outket = '\006' (* ACK *)
   (* 7 is bell
      8 is backspace
      9 is tab
      10 is lf
      11 is vt
      12 is ff
      13 is cr
    *)
let lockbra = '\014' (* SO *)
let lockket = '\015' (* SI *)
   
(* this function records a decision NEVER to put printable characters below space 
   (ASCII decimal 32) in a font.
 *)
let invisible_char c = (onbra <= c && c <= outket) || c = lockbra || c = lockket
let invisible s =
  not (List.exists (not <.> invisible_char) (chars_of_string s))

exception Catastrophe_ of string list
exception ParseError_ of string list
exception Tacastrophe_ of string list

