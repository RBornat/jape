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

open Miscellaneous
open Listfuns
open Sml

let isQuoted s =
  String.sub s 0 1 = "\"" && String.sub s (String.length s - 1) 1 = "\""

let disQuote s =
  try
    let size = String.length s in
    match String.sub s 0 1 with
      "\"" ->
        begin match String.sub s (size - 1) 1 with
          "\"" -> String.sub s 1 (size - 2)
        | _ -> s
        end
    | _ -> s
  with
    _ -> s

let enQuote s = 
  "\"" ^ implode (List.map (fun s -> if s="\"" then "\\\"" else s) (explode s)) ^ "\""
  
let enCharQuote s = 
  "'" ^ implode (List.map (fun s -> if s="'" then "\\'" else s) (explode s)) ^ "'"

let lowercase = String.lowercase_ascii
let uppercase = String.uppercase_ascii

let catelim_string_of_pair fa fb sep (a, b) tail =
  "(" :: fa a (sep :: fb b (")" :: tail))
let catelim_string_of_triple fa fb fc sep (a, b, c) tail =
  "(" :: fa a (sep :: fb b (sep :: fc c (")" :: tail)))
let catelim_string_of_quadruple fa fb fc fd sep (a, b, c, d) tail =
  "(" :: fa a (sep :: fb b (sep :: fc c (sep :: fd d (")" :: tail))))
let catelim_string_of_quintuple fa fb fc fd fe sep (a, b, c, d, e) tail =
  "(" :: fa a (sep :: fb b (sep :: fc c (sep :: fd d (sep :: fe e (")" :: tail)))))
let catelim_string_of_sextuple
  fa fb fc fd fe ff sep (a, b, c, d, e, f) tail =
  "(" :: fa a (sep :: fb b (sep :: fc c (sep :: fd d (sep :: fe e (sep :: ff f (")" :: tail))))))
let catelim_string_of_septuple
  fa fb fc fd fe ff fg sep (a, b, c, d, e, f, g) tail =
  "(" :: fa a (sep :: fb b (sep :: fc c (sep ::
         fd d (sep :: fe e (sep :: ff f (sep :: fg g (")" :: tail)))))))
let catelim_string_of_octuple
  fa fb fc fd fe ff fg fh sep (a, b, c, d, e, f, g, h) tail =
  "(" :: fa a (sep :: fb b (sep :: fc c (sep ::
         fd d (sep :: fe e (sep :: ff f (sep :: fg g (sep :: fh h (")" :: tail))))))))

let s = catelim_of_stringfn

let string_of_pair fa fb sep =
  stringfn_of_catelim (catelim_string_of_pair (s fa) (s fb) sep)
let string_of_triple fa fb fc sep =
  stringfn_of_catelim (catelim_string_of_triple (s fa) (s fb) (s fc) sep)
let string_of_quadruple fa fb fc fd sep =
  stringfn_of_catelim
    (catelim_string_of_quadruple (s fa) (s fb) (s fc) (s fd) sep)
let string_of_quintuple fa fb fc fd fe sep =
  stringfn_of_catelim
    (catelim_string_of_quintuple (s fa) (s fb) (s fc) (s fd) (s fe) sep)
let string_of_sextuple fa fb fc fd fe ff sep =
  stringfn_of_catelim
    (catelim_string_of_sextuple (s fa) (s fb) (s fc) (s fd) (s fe) (s ff) sep)
let string_of_septuple fa fb fc fd fe ff fg sep =
  stringfn_of_catelim
    (catelim_string_of_septuple (s fa) (s fb) (s fc) (s fd) (s fe) (s ff) (s fg) sep)
let string_of_octuple fa fb fc fd fe ff fg fh sep =
  stringfn_of_catelim
    (catelim_string_of_octuple (s fa) (s fb) (s fc) (s fd) (s fe) (s ff) (s fg) (s fh) sep)

let catelim_string_of_array f sep a ss =
  let rec el i ss =
    if i = Array.length a then ss
    else
      let doit ss = string_of_int i :: ": " :: f (Array.get a i) ss in
      doit (if i = Array.length a - 1 then ss else sep :: el (i + 1) ss)
  in
  "Ç" :: el 0 ("È" :: ss)

let string_of_array f sep =
  stringfn_of_catelim (catelim_string_of_array (s f) sep)

let quotedstring_of_char c = "'" ^ (Char.escaped c) ^ "'"

let hexdigs = "0123456789abcdef"

let fixedwidth_hexstring_of_int w i =
  let rec h w i cs = 
    if i=0 && w<=0 then cs else  h (w-1) (i lsr 4) (hexdigs.[i land 0xf] :: cs)
  in
  Sml.string_of_chars (h w i [])

let hexstring_of_int = fixedwidth_hexstring_of_int 1

let rec wordstring_of_int = function
 | 0 -> "zero"
 | 1 -> "one"
 | 2 -> "two"
 | 3 -> "three"
 | 4 -> "four"
 | 5 -> "five"
 | 6 -> "six"
 | 7 -> "seven"
 | 8 -> "eight"
 | 9 -> "nine"
 | n -> if n<0 && -10<n then "minus " ^ wordstring_of_int n else string_of_int n
 