/* 
    $Id$
    
    Copyright © 2003-5 Richard Bornat & Bernard Sufrin
	
	richard@bornat.me.uk
	sufrin@comlab.ox.ac.uk
    
    This file is part of the Jape GUI, which is part of Jape.
    
    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
    
    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with Jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).
    
*/

package uk.org.jape;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.MouseEvent;
import java.awt.Rectangle;

public	class SelectableProofItem extends TextSelectableItem
			       implements ProtocolConstants {
    private final boolean ambiguous;
    private byte selectionKind;

    public SelectableProofItem(ProofCanvas canvas, int x, int y, byte selectionKind, boolean ambiguous,
			       AnnotatedTextComponent[] components) {
	super(canvas,x,y,components);
	this.selectionKind = selectionKind; // ReasonSel, HypSel, ConcSel
	this.ambiguous = ambiguous;
	addSelectionIndicator(
	    canvas.proofStyle==BoxStyle ? new DirectedFormulaSelection(this) :
					  new RectSelection(this));
    }

    public void setSelectionKind(byte selectionKind) {
	if (ambiguous ? (selectionKind==HypSel || selectionKind==ConcSel) :
			selectionKind==this.selectionKind)
	    this.selectionKind = selectionKind;
	else
	    Alert.abort("SelectableProofItem.setSelectionKind("+selectionKind+")"+
			"; ambiguous="+ambiguous+"; selectionKind="+this.selectionKind);
    }

    public byte getSelectionKind() {
	return selectionKind;
    }

    public boolean getAmbiguous() {
	return ambiguous;
    }
    
    // single click can change perception of an ambiguous formula
    public void selectionclicked(byte eventKind, MouseEvent e) {
	if (ambiguous) {
	    byte newSelectionKind = e.getY()<getHeight()/2 ? ConcSel : HypSel;
	    if (newSelectionKind!=selectionKind) {
		setSelected(false);
		setSelectionKind(newSelectionKind);
	    }
	}
	super.selectionclicked(eventKind, e);
    }

    // you can't double-click an ambiguous unselected item
    public void selectiondoubleclicked(byte eventKind, MouseEvent e) {
	if (!ambiguous || getSelected())
	    super.selectiondoubleclicked(eventKind, e);
    }
}
