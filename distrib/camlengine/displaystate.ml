(* $Id$ *)

module type T =
  sig
    type prooftree and path and 'a hit and displayclass and hitkind and pos
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
  end


(* $Id$ *)

module M : T with type displayclass = Displayclass.M.displayclass
			  and type 'a hit        = 'a Hit.M.hit
			  and type hitkind       = Hit.M.hitkind
			  and type pos           = Box.M.pos
			  and type prooftree     = Prooftree.Tree.Fmttree.prooftree
			  and type path          = Prooftree.Tree.Fmttree.path
=
  struct
    type displayclass = Displayclass.M.displayclass
	 and 'a hit        = 'a Hit.M.hit
	 and hitkind       = Hit.M.hitkind
	 and pos           = Box.M.pos
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
  end


