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

open Box

type font = Displayfont.displayfont

type textalign = FirstLine | MidBlock | LastLine

type syllable = Syllable of (font * string)
              | Gap of int
              | Linebreak of int
              | Block of (textalign * syllable list)

type text = Text of syllable list

type textlayout = Textlayout of (pos * font * string) list

(* Offset positions are relative to a baseline of (0,0), 
 * so a single syllable will have an offset of (0,0).
 *) 

val text_of_string   : font -> string -> text
val measuretext      : (font -> string -> int * int * int) -> textalign -> text 
                    -> textsize * textlayout
val textlayoutOffset : textlayout -> pos -> textlayout

val string_of_textalign  : textalign -> string
val string_of_syllable   : syllable -> string
val string_of_text       : text -> string
val string_of_textlayout : textlayout -> string
