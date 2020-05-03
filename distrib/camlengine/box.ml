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

(* New version, November 94, cos I'm tired of the fact that text printing 
 * isn't described baseline-relative.  So now we have a size, which is
 * appropriate to a rectangular box, and a textsize, which is appropriate to
 * a box containing text.  
 * I can't decide whether the two sorts of size ought to be compatible - I 
 * shan't decide till I have seen what the uses of box, pos and size might be.
 *)

let ( <.> ) = Sml.( <.> )

type size = Size of (int * int) (* w,h *)

type textsize = Textsize of (int * int * int) (* w, ascent, descent *)

type pos = Pos of (int * int) (* x,y *)

type box = Box of (pos * size)

type textbox = Textbox of (pos * textsize)

let origin = Pos (0, 0)

let nullsize = Size (0, 0)

let nulltextsize = Textsize (0, 0, 0)

let emptybox = Box (origin, nullsize)

let emptytextbox = Textbox (origin, nulltextsize)

let downby (Pos (x, y)) h = Pos (x, y + h)

let rightby (Pos (x, y)) w = Pos (x + w, y)

let upby (Pos (x, y)) h = Pos (x, y - h)

let leftby (Pos (x, y)) w = Pos (x - w, y)

let ( +->+ ) (Pos (x, y)) (Pos (w, h)) = Pos (x + w, y + h) (* vector add *)

let ( +<-+ ) (Pos (x, y)) (Pos (w, h)) = Pos (x - w, y - h)

(* vector subtract *)

let posX (Pos (x, y)) = x

let posY (Pos (x, y)) = y

let sW (Size (w, h)) = w

let sH (Size (w, h)) = h

let tsW (Textsize (w, a, d)) = w

let tsH (Textsize (w, a, d)) = a + d

let tsA (Textsize (w, a, d)) = a

let tsD (Textsize (w, a, d)) = d

let bPos (Box (p, s)) = p

let bSize (Box (p, s)) = s

let tbP (Textbox (p, s)) = p

let tbS (Textbox (p, s)) = s

let tbH = tsH <.> tbS

let tbW = tsW <.> tbS

(* topleft, topright, botleft, botright: all positions within the box. Hence the -1s *)
let topleft = bPos

let topright (Box (p, s)) = rightby p (sW s - 1)

let botleft (Box (p, s)) = downby p (sH s - 1)

let botright (Box (p, s)) = rightby (downby p (sH s - 1)) (sW s - 1)

(* let the poor things make the values *)
let pos x y = Pos (x, y)

let size w h = Size (w, h)

let textsize w a d = Textsize (w, a, d)

let box pos size = Box (pos, size)

let textbox pos textsize = Textbox (pos, textsize)

(* and even let them see them *)
let rec string_of_xy = Stringfuns.string_of_pair string_of_int string_of_int ","

let rec string_of_wxy =
  Stringfuns.string_of_triple string_of_int string_of_int string_of_int ","

let rec string_of_pos (Pos p) = "Pos" ^ string_of_xy p

let rec string_of_size (Size p) = "Size" ^ string_of_xy p

let rec string_of_textsize (Textsize t) = "Textsize" ^ string_of_wxy t

let string_of_box (Box b) =
  "Box" ^ Stringfuns.string_of_pair string_of_pos string_of_size "," b

let string_of_textbox (Textbox tb) =
  "Textbox" ^ Stringfuns.string_of_pair string_of_pos string_of_textsize "," tb

let rec isemptybox b =
  let s = bSize b in
  sW s = 0 && sH s = 0

let rec isemptytextbox tb =
  let ts = tbS tb in
  tsW ts = 0 && tsH ts = 0

(* add together two text sizes, as if
 * putting two texts one after the other on the same line.
 *)
let ( +-+ ) (Textsize (w, a, d)) (Textsize (w', a', d')) =
  Textsize (w + w', max a a', max d d')

(* form the enclosing box *)
let ( +||+ ) (Box (Pos (x, y), Size (w, h)) as b)
    (Box (Pos (x', y'), Size (w', h')) as b') =
  if w = 0 && h = 0 then b'
  else if w' = 0 && h' = 0 then b
  else
    let minx = min x x' in
    let maxx = max (x + w) (x' + w') in
    let miny = min y y' in
    let maxy = max (y + h) (y' + h') in
    Box (Pos (minx, miny), Size (maxx - minx, maxy - miny))

(* not symmetrical - takes y position from first box, adjusts A, D to fit *)
(* unless either box is empty ... *)
let ( +|-|+ ) (Textbox (Pos (x, y), Textsize (w, a, d)) as b)
    (Textbox (Pos (x', y'), Textsize (w', a', d')) as b') =
  if w = 0 && a + d = 0 then b'
  else if w' = 0 && a' + d' = 0 then b
  else
    let minx = min x x' in
    let maxx = max (x + w) (x' + w') in
    let miny = min (y - a) (y' - a') in
    let maxy = max (y + d) (y' + d') in
    Textbox (Pos (minx, y), Textsize (maxx - minx, y - miny, maxy - y))

(* and convert from textsize to size, textbox to box, but never the other way *)
let size_of_textsize (Textsize (w, a, d)) = Size (w, a + d)

let box_of_textbox (Textbox (Pos (x, y), Textsize (w, a, d))) =
  Box (Pos (x, y - a), Size (w, a + d))

(* find whether a position is within a box *)
let withinX (Pos (x, y) (* as p *))
    (Box (Pos (x', y') (* as p' *), Size (w', h'))) =
  x' <= x && x < x' + w'

(* note x<x'+w: x'+w is outside the box *)
let withinY (Pos (x, y) (* as p *))
    (Box (Pos (x', y') (* as p'*), Size (w', h'))) =
  y' <= y && y < y' + h'

let within p b = withinX p b && withinY p b

let withintb p tb = within p (box_of_textbox tb)

(* find whether a box is within another box *)
let entirelywithin b1 b2 = within (topleft b1) b2 && within (botright b1) b2

let intersects (Box (Pos (x1, y1), Size (w1, h1)))
    (Box (Pos (x2, y2), Size (w2, h2))) =
  x1 < x2 + w2 && x2 < x1 + w1 && y1 < y2 + h2 && y2 < y1 + h1

let entirelywithintb b1 b2 =
  entirelywithin (box_of_textbox b1) (box_of_textbox b2)

(* compute enclosing (or enclosed box) with Outset; shifted box with Offset *)
let bOutset (Box (Pos (x, y), Size (w, h))) (Size (wa, ha)) =
  Box (Pos (x - wa, y - ha), Size (w + (wa * 2), h + (ha * 2)))

let tbOutset (Textbox (Pos (x, y), Textsize (w, a, d))) (Textsize (wa, aa, da))
    =
  Textbox (Pos (x - wa, y), Textsize (w + (wa * 2), a + aa, d + da))

let bOffset (Box (Pos (x, y), size)) (Pos (xa, ya)) =
  Box (Pos (x + xa, y + ya), size)

let tbOffset (Textbox (Pos (x, y), textsize)) (Pos (xa, ya)) =
  Textbox (Pos (x + xa, y + ya), textsize)

let nextright_of_textbox tb = rightby (tbP tb) (tsW (tbS tb))
