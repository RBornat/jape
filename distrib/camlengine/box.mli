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

type size
and textsize
and pos
and box
and textbox

(* build values *)
val size : int * int -> size (* w,h *)
val textsize : int * int * int -> textsize (* w, ascent, descent *)
val textsize2size : textsize -> size
val pos : int * int -> pos (* x,y *)
val box : pos * size -> box
val textbox : pos * textsize -> textbox
val textbox2box : textbox -> box

(* take them apart *)
val sW : size -> int
val sH : size -> int
val tsW : textsize -> int
val tsH : textsize -> int
val tsA : textsize -> int
val tsD : textsize -> int
val posX : pos -> int
val posY : pos -> int
val bSize : box -> size
val bPos : box -> pos
val tbSize : textbox -> textsize
val tbPos : textbox -> pos

(* extra conveniences for boxes: note that these points are all inside the box.
 * that is, topright = topleft rightby width-1, botleft = topleft downby height-1,
 * and botright follows.
 *)
val topleft : box -> pos
val topright : box -> pos
val botleft : box -> pos
val botright : box -> pos

(* operators on these types
     infix downby rightby upby leftby +->+ +<-+ +-+ +||+ +|-|+
           withinX withinY within withintb entirelywithin entirelywithintb

 *)
val ( +-+ ) : textsize * textsize -> textsize
(* as if appending text on a line *)
val ( +->+ ) : pos * pos -> pos
(* as if offsetting a position right and down *)
val ( +<-+ ) : pos * pos -> pos
(* ditto left and up *)
val ( +||+ ) : box * box -> box
(* find the bounding box *)
val ( +|-|+ ) : textbox * textbox -> textbox
(* find the bounding box - takes y pos of first argument *)
 
val emptybox : box
val emptytextbox : textbox
val nullsize : size
val nulltextsize : textsize
val origin : pos
val downby : pos * int -> pos
val rightby : pos * int -> pos
val upby : pos * int -> pos
val leftby : pos * int -> pos

(* enquiries. within can reasonably be infix *)
val withinX : pos * box -> bool
val withinY : pos * box -> bool
val within : pos * box -> bool
val withintb : pos * textbox -> bool
val entirelywithin : box * box -> bool
val entirelywithintb : textbox * textbox -> bool
val isemptybox : box -> bool
val isemptytextbox : textbox -> bool

(* box modifiers - Outset gives space round a box; Offset moves its position *)
val bOutset : box -> size -> box
val tbOutset : textbox -> textsize -> textbox (* doesn't move y position *)
val bOffset : box -> pos -> box
val tbOffset : textbox -> pos -> textbox

(* show me *)
val posstring : pos -> string
val sizestring : size -> string
val textsizestring : textsize -> string
val boxstring : box -> string
val textboxstring : textbox -> string
