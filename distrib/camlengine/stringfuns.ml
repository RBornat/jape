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

open Miscellaneous
open Listfuns
open Sml

let rec isQuoted s =
  String.sub s 0 1 = "\"" && String.sub s (String.length s - 1) 1 = "\""

let rec disQuote s =
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

let enQuote s = "\"" ^ s ^ "\""
let enCharQuote s = "'" ^ s ^ "'"

let lowercase = String.lowercase
let uppercase = String.uppercase

let rec catelim_pairstring fa fb sep (a, b) tail =
  "(" :: fa a (sep :: fb b (")" :: tail))
let rec catelim_triplestring fa fb fc sep (a, b, c) tail =
  "(" :: fa a (sep :: fb b (sep :: fc c (")" :: tail)))
let rec catelim_quadruplestring fa fb fc fd sep (a, b, c, d) tail =
  "(" :: fa a (sep :: fb b (sep :: fc c (sep :: fd d (")" :: tail))))
let rec catelim_quintuplestring fa fb fc fd fe sep (a, b, c, d, e) tail =
  "(" :: fa a (sep :: fb b (sep :: fc c (sep :: fd d (sep :: fe e (")" :: tail)))))
let rec catelim_sextuplestring
  fa fb fc fd fe ff sep (a, b, c, d, e, f) tail =
  "(" :: fa a (sep :: fb b (sep :: fc c (sep :: fd d (sep :: fe e (sep :: ff f (")" :: tail))))))
let rec catelim_septuplestring
  fa fb fc fd fe ff fg sep (a, b, c, d, e, f, g) tail =
  "(" :: fa a (sep :: fb b (sep :: fc c (sep ::
         fd d (sep :: fe e (sep :: ff f (sep :: fg g (")" :: tail)))))))
let rec catelim_octuplestring
  fa fb fc fd fe ff fg fh sep (a, b, c, d, e, f, g, h) tail =
  "(" :: fa a (sep :: fb b (sep :: fc c (sep ::
         fd d (sep :: fe e (sep :: ff f (sep :: fg g (sep :: fh h (")" :: tail))))))))

let s = stringfn2catelim

let rec pairstring fa fb sep =
  catelim2stringfn (catelim_pairstring (s fa) (s fb) sep)
let rec triplestring fa fb fc sep =
  catelim2stringfn (catelim_triplestring (s fa) (s fb) (s fc) sep)
let rec quadruplestring fa fb fc fd sep =
  catelim2stringfn
    (catelim_quadruplestring (s fa) (s fb) (s fc) (s fd) sep)
let rec quintuplestring fa fb fc fd fe sep =
  catelim2stringfn
    (catelim_quintuplestring (s fa) (s fb) (s fc) (s fd) (s fe) sep)
let rec sextuplestring fa fb fc fd fe ff sep =
  catelim2stringfn
    (catelim_sextuplestring (s fa) (s fb) (s fc) (s fd) (s fe) (s ff) sep)
let rec septuplestring fa fb fc fd fe ff fg sep =
  catelim2stringfn
    (catelim_septuplestring (s fa) (s fb) (s fc) (s fd) (s fe) (s ff) (s fg) sep)
let rec octuplestring fa fb fc fd fe ff fg fh sep =
  catelim2stringfn
    (catelim_octuplestring (s fa) (s fb) (s fc) (s fd) (s fe) (s ff) (s fg) (s fh) sep)

let rec catelim_arraystring f sep a ss =
  let rec el i ss =
    if i = Array.length a then ss
    else
      let rec doit ss = string_of_int i :: ": " :: f (Array.get a i) ss in
      doit (if i = Array.length a - 1 then ss else sep :: el (i + 1) ss)
  in
  "Ç" :: el 0 ("È" :: ss)

let rec arraystring f sep =
  catelim2stringfn (catelim_arraystring (s f) sep)

let quotedstring_of_char c = "'" ^ (Char.escaped c) ^ "'"

let hexdigs = "0123456789abcdef"

let fixedwidth_hexstring_of_int w i =
  let rec h w i cs = 
    if i=0 && w<=0 then cs else  h (w-1) (i lsr 4) (hexdigs.[i land 0xf] :: cs)
  in
  Sml.string_of_chars (h w i [])

let hexstring_of_int = fixedwidth_hexstring_of_int 1
