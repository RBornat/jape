(* $Id$ *)

module type T =
  sig
    type tree = Absprooftree.tree
    and 'a hit = 'a Hit.hit
    and displayclass = Displayclass.displayclass
    and hitkind = Hit.hitkind
    and pos = Box.pos
    and size = Box.size
    and box = Box.box
    and textbox = Box.textbox
    and layout
    
    val clearView : unit -> unit val viewBox : unit -> box
    val layout : tree -> layout
    val defaultpos : layout -> pos
    val rootpos : layout -> pos
    val postoinclude : box -> layout -> pos
    val draw : int list option -> pos -> tree -> layout -> unit
    val print : out_channel -> int list option -> pos -> tree -> layout -> unit
    val locateHit :
      pos -> displayclass option -> hitkind -> pos * tree * layout ->
        int list hit option
    val refineSelection : bool
    val notifyselect :
      (pos * displayclass) option -> (pos * displayclass) list ->
        pos * tree * layout -> unit
    val highlight : pos -> displayclass option -> unit
    val targetbox : int list option -> layout -> textbox option
    val samelayout : layout * layout -> bool
  end


