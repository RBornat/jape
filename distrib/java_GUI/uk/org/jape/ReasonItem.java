/*
    $Id$
    
    Copyright © 2002 Richard Bornat & Bernard Sufrin
    
    richard@bornat.me.uk
    sufrin@comlab.ox.ac.uk
    
    This file is part of japeserver, which is part of jape.
    
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

  */

import java.awt.event.MouseEvent;

public class ReasonItem extends SelectableProofItem {

    public ReasonItem(ProofCanvas canvas, int x, int y, byte fontnum, String annottext) {
        super(canvas, x, y, fontnum, annottext);
    }

    public void clicked(byte eventKind, MouseEvent e) {
        canvas.claimFocus();
        if (selectionRect.getSelkind()==NoSel) {
            canvas.killSelections((byte)(ReasonSel | HypSel | ConcSel));
            selectionRect.setSelkind(ReasonSel);
            canvas.notifySelect(this);
        }
        else
            switch (eventKind) {
                case ExtendedSelection:
                case DisjointSelection:
                case ExtendedDisjointSelection:
                    selectionRect.setSelkind(NoSel);
                    canvas.notifyDeselect();
                    break;
            }
    }

    public void select(byte selkind) {
        if (selkind==ReasonSel)
            selectionRect.setSelkind(ReasonSel);
        else
            Alert.abort("ReasonItem.select selkind="+selkind);
    }
}
