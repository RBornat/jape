(* $Id$ *)

module type Displaystate =
  sig
    type 'a prooftree
    and treeformat
    and fmtpath
    and 'a hit
    and displayclass
    and hitkind
    and pos
    type displaystate =
        DisplayState of
          < showProof
              :
              treeformat prooftree -> fmtpath option -> fmtpath option ->
                displaystate;
            showFocussedProof
              :
              treeformat prooftree -> fmtpath option -> displaystate;
            refreshProof : unit -> unit;
            printProof
              :
              Pervasives.out_channel -> treeformat prooftree -> fmtpath option ->
                fmtpath option -> unit;
            locateHit
              :
              pos -> displayclass option -> hitkind -> fmtpath hit option;
            refineSelection : bool;
            notifyselect
              :
              (pos * displayclass) option -> (pos * displayclass) list ->
                unit;
            storedProof : unit -> treeformat prooftree option >
  end


(* $Id$ *)

module
  Displaystate
  (AAA :
    sig
      type 'a hit
      and hitkind
      and pos
      and 'a prooftree
      and treeformat
      and fmtpath
      and displayclass
    end)
  :
  Displaystate =
  struct
    open AAA
    type 'a hit = 'a hit
    and hitkind = hitkind
    and pos = pos
    and 'a prooftree = 'a prooftree
    and treeformat = treeformat
    and fmtpath = fmtpath
    and displayclass = displayclass
    open IO
    type displaystate =
        DisplayState of
          < showProof
              :
              treeformat prooftree -> fmtpath option -> fmtpath option ->
                displaystate;
            showFocussedProof
              :
              treeformat prooftree -> fmtpath option -> displaystate;
            refreshProof : unit -> unit;
            printProof
              :
              Pervasives.out_channel -> treeformat prooftree -> fmtpath option ->
                fmtpath option -> unit;
            locateHit
              :
              pos -> displayclass option -> hitkind -> fmtpath hit option;
            refineSelection : bool;
            notifyselect
              :
              (pos * displayclass) option -> (pos * displayclass) list ->
                unit;
            storedProof : unit -> treeformat prooftree option >
  end


