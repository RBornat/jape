(* $Id$ *)

type text = Text.M.text
and textlayout = Text.M.textlayout
and textalign = Text.M.textalign
and textsize = Box.textsize
and pos = Box.pos
and size = Box.size
and box = Box.box
and textbox = Box.textbox
and font = Text.M.font
and displayclass = Displayclass.displayclass
and element = Term.Funs.element
and term = Term.Funs.term
and reason = Absprooftree.reason

type 'a plan = Formulaplan of (textlayout * textbox * 'a)

val measuretext : textalign -> text -> textsize * textlayout
val drawinproofpane : unit -> unit
val drawLine : box -> unit
val drawBox : box -> unit
val clearView : unit -> unit
val viewBox : unit -> box
val highlight : pos -> displayclass option -> unit
val greyen : pos -> unit
val blacken : pos -> unit
val fontinfo : font -> int * int * int (* ascent, descent, leading *)
val linethickness : int -> int (* font leading to linethickness *)
   
val setproofparams : string -> int -> unit (* "tree"/"box", line thickness *)
   
val planstring : ('a -> string) -> 'a plan -> string (* for debugging *)
val plan2string : 'a plan -> string (* for external viewing; only works for single-string plans *)
   
val plantextlayout : 'a plan -> textlayout
val plantextbox : 'a plan -> textbox
val planinfo : 'a plan -> 'a
val plantextsize : 'a plan -> textsize
val planlisttextsize : 'a plan list -> textsize
val text2textinfo : text -> textsize * textlayout
val string2textinfo : font -> string -> textsize * textlayout
val term2textinfo : (term -> string) -> term -> textsize * textlayout
val element2textinfo : (element -> string) -> element -> textsize * textlayout
val reason2textinfo : reason -> textsize * textlayout
val procrustean_reason2textinfo : int -> reason -> textsize * textlayout
val textinfo2plan : textsize * textlayout -> 'a -> pos -> 'a plan
val string2plan : font -> string -> 'a -> pos -> 'a plan
val element2plan : (element -> string) -> element -> 'a -> pos -> 'a plan

(* for building element lists and the like, with commas in between *)
val plancons : 'a plan -> (pos -> 'a plan list * textbox) -> 'a plan list * textbox
val plan2plans : 'a plan -> 'a plan list * textbox

(* plan modifier *)
val planOffset : 'a plan -> pos -> 'a plan
val things2plans :
  ('b -> pos -> 'a plan) -> (pos -> 'a plan) ->
    (pos -> 'a plan list * textbox) -> 'b list -> pos ->
    'a plan list * textbox

val drawplan : ('a -> displayclass) -> pos -> 'a plan -> unit
val findfirstplanhit : pos -> 'a plan list -> 'a plan option

val textinfostring : textsize * textlayout -> string
