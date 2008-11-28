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

open Box
open Miscellaneous
open Listfuns
open Displayfont
open Stringfuns
    
type font = Displayfont.displayfont

let string_of_font = string_of_displayfont

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
let string_of_textalign =
  function FirstLine -> "FirstLine"
  |        MidBlock  -> "MidBlock"
  |        LastLine  -> "LastLine"

let rec string_of_syllable =
  function
    Syllable (i, s) -> "Syllable(" ^ string_of_font i ^ "," ^ enQuote s ^ ")"
  | Gap i           -> "Gap " ^ string_of_int i
  | Linebreak i     -> "Linebreak " ^ string_of_int i
  | Block (c, sys)  -> "Block(" ^ string_of_textalign c ^ "," ^
                          bracketedstring_of_list string_of_syllable "," sys ^ ")"

let string_of_text =
  fun (Text sys) -> "Text" ^ bracketedstring_of_list string_of_syllable "," sys

let string_of_textlayout =
  fun (Textlayout t) ->
    "TextLayout" ^
      bracketedstring_of_list
        (string_of_triple string_of_pos string_of_font enQuote ",") ", " t

let text_of_string font string = Text [Syllable (font, string)]

(* This function doesn't try to do anything clever with leading or trailing Gaps or
 * Linebreaks.  So don't use them, if you don't want silly things to happen.
 *)
(* It gives back the string list in the wrong order, but measuretext can rev it if it wants to *)
let rec textWAD measure c ss sys =
  let rec pushdown d ((x, y), string_of_font) = (x, y + d), string_of_font in
  let rec f a1 a2 a3 a4 a5 a6 =
    match a1, a2, a3, a4, a5, a6 with
      w, a, d, x, ss, []                      -> (w, a, d), ss
    | w, a, d, x, ss, Syllable (_, "") :: sys -> f w a d x ss sys
    | w, a, d, x, ss, Syllable s       :: sys -> add3 w a d x (measure s) (((x, 0), s) :: ss) sys
    | w, a, d, x, ss, Gap i            :: sys -> f (w + i) a d (x + i) ss sys
    | w, a, d, x, ss, Block (c', sys') :: sys ->
        let (wad, ss') = textWAD measure c' ss sys' in add3 w a d x wad ss' sys
    | w, a, d, x, ss, Linebreak i      :: sys ->
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

let measuretext measure c (Text sys) =
    let ((w,a,d), ss) = textWAD (uncurry2 measure) c [] sys in
    textsize w a d,
    Textlayout (List.map (fun ((x,y), (f, s)) -> pos x y, f, s) (List.rev ss))

let textlayoutOffset (Textlayout ts) pos =
    Textlayout (List.map (fun (p, f, s) -> pos +->+ p, f, s) ts)
