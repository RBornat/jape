(* $Id$ *)

module type Box =
  sig
    type size and textsize and pos and box and textbox
    (* build values *)
    val size : int * int -> size
    (* w,h *)
    val textsize : int * int * int -> textsize
    (* w, ascent, descent *)
    val textsize2size : textsize -> size
    (* x,y *)
    val pos : int * int -> pos
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
    val tbOutset : textbox -> textsize -> textbox
    (* doesn't move y position *)
    val bOffset : box -> pos -> box
    val tbOffset : textbox -> pos -> textbox
    (* show me *)
    val posstring : pos -> string
    val sizestring : size -> string
    val textsizestring : textsize -> string
    val boxstring : box -> string
    val textboxstring : textbox -> string
  end
(* $Id$ *)

module Box : Box =
  struct
    (* the algebra of Pos and Box deserves a functor, I think *)
    (* New version, November 94, cos I'm tired of the fact that text printing 
     * isn't described baseline-relative.  So now we have a size, which is
     * appropriate to a rectangular box, and a textsize, which is appropriate to
     * a box containing text.  
     * I can't decide whether the two sorts of size ought to be compatible - I 
     * shan't decide till I have seen what the uses of box, pos and size might be.
     *)
     
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
    
    let rec downby = fun (Pos (x, y), h) -> Pos (x, y + h)
    let rec rightby = fun (Pos (x, y), w) -> Pos (x + w, y)
    let rec upby = fun (Pos (x, y), h) -> Pos (x, y - h)
    let rec leftby = fun (Pos (x, y), w) -> Pos (x - w, y)
    let rec ( +->+ ) = fun (Pos (x, y), Pos (w, h)) -> Pos (x + w, y + h)
    (* vector add *)
    let rec ( +<-+ ) = fun (Pos (x, y), Pos (w, h)) -> Pos (x - w, y - h)
    (* vector subtract *)
       
    let rec posX = fun (Pos (x, y)) -> x
    let rec posY = fun (Pos (x, y)) -> y
    let rec sW = fun (Size (w, h)) -> w
    let rec sH = fun (Size (w, h)) -> h
    let rec tsW = fun (Textsize (w, a, d)) -> w
    let rec tsH = fun (Textsize (w, a, d)) -> a + d
    let rec tsA = fun (Textsize (w, a, d)) -> a
    let rec tsD = fun (Textsize (w, a, d)) -> d
    let rec bPos = fun (Box (p, s)) -> p
    let rec tbPos = fun (Textbox (p, s)) -> p
    let rec bSize = fun (Box (p, s)) -> s
    let rec tbSize = fun (Textbox (p, s)) -> s
    (* topleft, topright, botleft, botright: all positions within the box. Hence the -1s *)
    let topleft = bPos
    let rec topright = fun (Box (p, s)) -> rightby (p, sW s - 1)
    let rec botleft = fun (Box (p, s)) -> downby (p, sH s - 1)
    let rec botright =
      fun (Box (p, s)) -> rightby (downby (p, sH s - 1), sW s - 1)
    (* let the poor things make the values *)
    let pos v = Pos v
    let size v = Size v
    let textsize v = Textsize v
    let box v = Box v
    let textbox v = Textbox v
    (* and even let them see them *)
    let rec pairstring ((x : int), (y : int)) =
      ((("(" ^ string_of_int x) ^ ",") ^ string_of_int y) ^ ")"
    let rec triplestring ((w : int), (x : int), (y : int)) =
      ((((("(" ^ string_of_int w) ^ ",") ^ string_of_int x) ^ ",") ^ string_of_int y) ^
        ")"
    let rec posstring = fun (Pos p) -> "Pos" ^ pairstring p
    let rec sizestring = fun (Size p) -> "Size" ^ pairstring p
    let rec textsizestring = fun (Textsize t) -> "Textsize" ^ triplestring t
    let rec boxstring =
      fun (Box (p, s)) ->
        ((("Box(" ^ posstring p) ^ ",") ^ sizestring s) ^ ")"
    let rec textboxstring =
      fun (Textbox (p, s)) ->
        ((("Textbox(" ^ posstring p) ^ ",") ^ textsizestring s) ^ ")"
    
    (* One thing we often want to do is to add together two text sizes, as if
     * putting two texts one after the other on the same line.
     *)
    let rec ( +-+ ) =
      fun (Textsize (w, a, d), Textsize (w', a', d')) ->
        Textsize (w+w', max (a) (a'), max (d) (d'))
    
    (* Given two boxes, we can form the enclosing box of the two *)
    let rec ( +||+ ) =
      fun
        (Box (Pos (x, y), Size (w, h)), Box (Pos (x', y'), Size (w', h'))) ->
        let minx = min (x) (x') in
        let maxx = max (x+w) (x'+w') in
        let miny = min (y) (y') in
        let maxy = max (y+h) (y'+h') in
        Box (Pos (minx, miny), Size (maxx - minx, maxy - miny))
    
    (* not symmetrical - takes y position from first box, adjusts A, D to fit *)
    let rec ( +|-|+ ) =
      fun
        (Textbox (Pos (x, y), Textsize (w, a, d)),
         Textbox (Pos (x', y'), Textsize (w', a', d'))) ->
        let minx = min (x) (x') in
        let maxx = max (x+w) (x'+w') in
        let miny = min (y - a) (y' - a') in
        let maxy = max (y+d) (y'+d') in
        Textbox (Pos (minx, y), Textsize (maxx - minx, y - miny, maxy - y))
    (* and we have to convert from textsize to size, textbox to box, but never the other way *)
    let rec textsize2size = fun (Textsize (w, a, d)) -> Size (w, a + d)
    let rec textbox2box =
      fun (Textbox (Pos (x, y), Textsize (w, a, d))) ->
        Box (Pos (x, y - a), Size (w, a + d))
    
    (* find whether a position is within a box *)
    let rec withinX =
      fun ((Pos (x, y) as p), Box ((Pos (x', y') as p'), Size (w', h'))) ->
        x' <= x && x < x' + w'
    (* note x<x'+w: x'+w is outside the box *)
    let rec withinY =
      fun ((Pos (x, y) as p), Box ((Pos (x', y') as p'), Size (w', h'))) ->
        y' <= y && y < y' + h'
    let rec within (p, b) = withinX (p, b) && withinY (p, b)
    let rec withintb (p, tb) = within (p, textbox2box tb)
    
    (* find whether a box is within another box *)
    let rec entirelywithin (b1, b2) =
      within (topleft b1, b2) && within (botright b1, b2)
    let rec entirelywithintb (b1, b2) =
      entirelywithin (textbox2box b1, textbox2box b2)
    (* compute enclosing (or enclosed box) with Outset; shifted box with Offset *)
    let rec bOutset =
      fun (Box (Pos (x, y), Size (w, h))) ->
        fun (Size (wa, ha)) ->
          Box (Pos (x - wa, y - ha), Size (w + wa * 2, h + ha * 2))
    let rec tbOutset =
      fun (Textbox (Pos (x, y), Textsize (w, a, d))) ->
        fun (Textsize (wa, aa, da)) ->
          Textbox (Pos (x - wa, y), Textsize (w + wa * 2, a + aa, d + da))
    let rec bOffset =
      fun (Box (Pos (x, y), size)) ->
        fun (Pos (xa, ya)) -> Box (Pos (x + xa, y + ya), size)
    let rec tbOffset =
      fun (Textbox (Pos (x, y), textsize)) ->
        fun (Pos (xa, ya)) -> Textbox (Pos (x + xa, y + ya), textsize)
    let rec isemptybox b = let s = bSize b in sW s = 0 && sH s = 0
    let rec isemptytextbox tb = let ts = tbSize tb in tsW ts = 0 && tsH ts = 0
  end
