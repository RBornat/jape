(* $Id$ *)

open Box

type font = Displayfont.displayfont

type textalign = FirstLine | MidBlock | LastLine
type syllable =
    Syllable of (font * string)
  | Gap of int
  | Linebreak of int
  | Block of (textalign * syllable list)
type text = Text of syllable list
type textlayout = Textlayout of (pos * font * string) list
(* Offset positions are relative to a baseline of (0,0), 
 * so a single syllable will have an offset of (0,0).
 *) 

val string2text : font -> string -> text
val measuretext :
  (font * string -> int * int * int) -> textalign -> text ->
    textsize * textlayout
val textlayoutOffset : textlayout -> pos -> textlayout
val textalignstring : textalign -> string
val syllablestring : syllable -> string
val textstring : text -> string
val textlayoutstring : textlayout -> string
