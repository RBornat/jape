(*
	$Id$

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

open Box
open Miscellaneous
open Listfuns
open Displayfont
open Stringfuns
    
type font = Displayfont.displayfont

let fontstring = displayfontstring

type textalign = FirstLine | MidBlock | LastLine
type syllable =
    Syllable of (font * string)
  | Gap of int
  | Linebreak of int
  | Block of (textalign * syllable list)
type text = Text of syllable list
type textlayout = Textlayout of (pos * font * string) list
(* Oh, I hate having to write all this.  Why can't the compiler synthesise it
 * for me?
 *)
let rec textalignstring =
  function
    FirstLine -> "FirstLine"
  | MidBlock -> "MidBlock"
  | LastLine -> "LastLine"
let rec syllablestring =
  function
    Syllable (i, s) -> ((("Syllable(" ^ fontstring i) ^ ",") ^ s) ^ ")"
  | Gap i -> "Gap " ^ string_of_int i
  | Linebreak i -> "Linebreak " ^ string_of_int i
  | Block (c, sys) ->
      ((("Block(" ^ textalignstring c) ^ ",") ^
         bracketedliststring syllablestring "," sys) ^
        ")"
let rec textstring =
  fun (Text sys) -> "Text" ^ bracketedliststring syllablestring "," sys
let rec textlayoutstring =
  fun (Textlayout t) ->
    "TextLayout" ^
      bracketedliststring
        (triplestring posstring fontstring (fun s -> s) ",") ", " t
let rec string2text font string = Text [Syllable (font, string)]
(* This function doesn't try to do anything clever with leading or trailing Gaps or
 * Linebreaks.  So don't use them, if you don't want silly things to happen.
 *)
(* It gives back the string list in the wrong order, but measuretext can rev it if it wants to *)
let rec textWAD measure c ss sys =
  let rec pushdown d ((x, y), fontstring) = (x, y + d), fontstring in
  let rec f a1 a2 a3 a4 a5 a6 =
    match a1, a2, a3, a4, a5, a6 with
      w, a, d, x, ss, [] -> (w, a, d), ss
    | w, a, d, x, ss, Syllable (_, "") :: sys -> f w a d x ss sys
    | w, a, d, x, ss, Syllable s :: sys ->
        add3 w a d x (measure s) (((x, 0), s) :: ss) sys
    | w, a, d, x, ss, Gap i :: sys -> f (w + i) a d (x + i) ss sys
    | w, a, d, x, ss, Block (c', sys') :: sys ->
        let (wad, ss') = textWAD measure c' ss sys' in
        add3 w a d x wad ss' sys
    | w, a, d, x, ss, Linebreak i :: sys ->
        let ((w', a', d'), ss') = f 0 0 0 0 [] sys in
        let nw = max w w' in
        match c with
          FirstLine ->
            (nw, a, d + i + a' + d'), List.map (pushdown (d + i + a')) ss' @ ss
        | MidBlock ->
            let h = a + d + i + a' + d' in
            let m = h / 2 in
            let nss = List.map (pushdown (- (m - a))) ss in
            let nss' = List.map (pushdown (h - m - d')) ss' in
            (nw, h / 2, h - h / 2), nss' @ nss
        | LastLine ->
            (nw, a + d + i + a', d'),
            ss' @ List.map (pushdown (- (d + i + a'))) ss
  and add3 w a d x (w', a', d') =
    f (w + w') (max a a') (max d d') (x + w')
  in
  f 0 0 0 0 ss sys
let rec measuretext measure c =
  fun (Text sys) ->
    let (wad, ss) = textWAD measure c [] sys in
    textsize wad,
    Textlayout (List.map (fun (xy, (f, s)) -> pos xy, f, s) (List.rev ss))
let rec textlayoutOffset =
  fun (Textlayout ts) pos ->
    Textlayout (List.map (fun (p, f, s) -> ( +->+ ) (pos, p), f, s) ts)
