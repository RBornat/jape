(* $Id$ *)

type displayclass = Displayclass.displayclass
 and 'a hit        = 'a Hit.hit
 and hitkind       = Hit.hitkind
 and pos           = Box.pos
 and prooftree     = Prooftree.Tree.Fmttree.prooftree
 and path          = Prooftree.Tree.Fmttree.path

type displaystaterec =
      { showProof         : prooftree -> path option -> path option -> displaystate;
        showFocussedProof : prooftree -> path option -> displaystate;
        refreshProof      : unit -> unit;
        printProof        : out_channel -> prooftree -> path option -> path option -> unit;
        locateHit         : pos -> displayclass option -> hitkind -> path hit option;
        refineSelection   : bool;
        notifyselect      : (pos * displayclass) option -> (pos * displayclass) list -> unit;
        storedProof       : unit -> prooftree option }

 and displaystate = DisplayState of displaystaterec


