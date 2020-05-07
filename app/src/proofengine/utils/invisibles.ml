(*
    Copyright (C) 2003-19 Richard Bornat & Bernard Sufrin
     
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

(* this collection avoids low characters which might be useful *)
(* 0 is NUL; can't appear in a C string, so don't use *)
let onbra = 0x01 (* SOH *)

let onket = 0x02 (* STX *)

let offbra = 0x03 (* ETX *)

let offket = 0x04 (* EOT *)

let outbra = 0x05 (* ENQ *)

let outket = 0x06 (* ACK *)

(* 7 is bell
   8 is backspace
   9 is tab
   10 is lf
   11 is vt
   12 is ff
   13 is cr
*)
let lockbra = 0x14 (* SO *)

let lockket = 0x15 (* SI *)

let onbra_as_string = "\x01" (* SOH *)

let onket_as_string = "\x02" (* STX *)

let offbra_as_string = "\x03" (* ETX *)

let offket_as_string = "\x04" (* EOT *)

let outbra_as_string = "\x05" (* ENQ *)

let outket_as_string = "\x06" (* ACK *)

(* 7 is bell
   8 is backspace
   9 is tab
   10 is lf
   11 is vt
   12 is ff
   13 is cr
*)
let lockbra_as_string = "\x14" (* SO *)

let lockket_as_string = "\x15" (* SI *)

let invisibles = Array.make 0x20 false

let _ =
  invisibles.(onbra) <- true;
  invisibles.(onket) <- true;
  invisibles.(offbra) <- true;
  invisibles.(offket) <- true;
  invisibles.(outbra) <- true;
  invisibles.(outket) <- true;
  invisibles.(lockbra) <- true;
  invisibles.(lockket) <- true

(* this function records a decision NEVER to put printable characters below space 
   (ASCII decimal 32) in a font.
 *)
let isInvisibleUcode c = try invisibles.(c) with _ -> false

let isInvisibleString s =
  let lim = String.length s in
  let rec f i =
    i = lim
    || isInvisibleUcode (utf8_get s i)
       (* won't be true if width 0 *) && f (i + utf8width_from_header s.[i])
  in
  f 0

let isInvisibleBra s =
  List.mem s
    [ onbra_as_string; offbra_as_string; outbra_as_string; lockbra_as_string ]

let isInvisibleKet s =
  List.mem s
    [ onket_as_string; offket_as_string; outket_as_string; lockket_as_string ]

let showInvisibleChar c =
  match c with
  | c when Char.code c = onbra -> "$ON("
  | c when Char.code c = onket -> ")ON$"
  | c when Char.code c = offbra -> "$OFF("
  | c when Char.code c = offket -> ")OFF$"
  | c when Char.code c = outbra -> "$OUT("
  | c when Char.code c = outket -> ")OUT$"
  | c when Char.code c = lockbra -> "$LOCK("
  | c when Char.code c = lockket -> ")LOCK$"
  | c -> String.make 1 c

let showInvisibleString =
  implode <.> List.map showInvisibleChar <.> chars_of_string
