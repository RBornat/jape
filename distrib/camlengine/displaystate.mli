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

type prooftree = Prooftree.Tree.Fmttree.prooftree
and path = Prooftree.Tree.Fmttree.path
and 'a hit = 'a Hit.hit
and displayclass = Displayclass.displayclass
and hitkind = Hit.hitkind
and pos = Box.pos

type displaystaterec = {
  showProof : prooftree -> path option -> path option -> displaystate;
  showFocussedProof : prooftree -> path option -> displaystate;
  refreshProof : unit -> unit;
  printProof :
    out_channel -> prooftree -> path option -> path option -> unit;
  locateHit : pos -> displayclass option -> hitkind -> path hit option;
  refineSelection : bool;
  notifyselect :
    (pos * displayclass) option -> (pos * displayclass) list -> unit;
  storedProof : unit -> prooftree option;
} 

and displaystate = DisplayState of displaystaterec
