/* 
    $Id$

    Copyright © 2003-4 Richard Bornat & Bernard Sufrin
     
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

import java.awt.event.MouseEvent;
import java.awt.Component;

public class DisplayItem extends Component
		      implements SelectionConstants {
    public final JapeCanvas canvas;
    public final int idX, idY;

    private boolean selected = false;
    protected SelectionIndicator indicator = null;
    
    protected DisplayItem(JapeCanvas canvas, int x, int y) {
	super();
	this.canvas = canvas; this.idX = x; this.idY = y;
    }

    // DisplayItems can have a selection halo; if they have an indicator that defines a halo
    protected int selectionHalo = 0;

    public boolean contains(int x, int y) {
	return super.contains(x,y) ||
		(indicator!=null && indicator.contains(x,y)) ||
		(selectionHalo>0 && 
		 x+selectionHalo>=0 && x-selectionHalo<getWidth() &&
		 y+selectionHalo>=0 && y-selectionHalo<getHeight());
    }

    // because of the halo, mouse coordinates may be outside the item

    protected int internalX(int px) {
	return Math.max(0, Math.min(getWidth(), px));
    }

    protected int internalY(int py) {
	return Math.max(0, Math.min(getHeight(), py));
    }

    // a DisplayItem can be enabled ('blackened') or disabled ('greyened')

    public void setEnabled(boolean enabled) {
	super.setEnabled(enabled);
	if (enabled)
	    enableEvents(java.awt.AWTEvent.MOUSE_EVENT_MASK);
	else {
	    setSelected(false);
	    disableEvents(java.awt.AWTEvent.MOUSE_EVENT_MASK);
	}
	repaint();
    }
    
    public void addJapeMouseListener(JapeMouseAdapter a) {
	addMouseListener(a); addMouseMotionListener(a);
    }

    public void removeJapeMouseListener(JapeMouseAdapter a) {
	removeMouseListener(a); removeMouseMotionListener(a);
    }

    private JapeMouseTextAdapter selectionListener = null;

    public void addSelectionIndicator(SelectionIndicator indicator) {
	if (selectionListener!=null)
	    removeJapeMouseListener(selectionListener);
	if (this.indicator!=null)
	    canvas.remove((Component)this.indicator);
	selectionListener = new JapeMouseTextAdapter() {
	    public void clicked(byte eventKind, MouseEvent e) {
		DisplayItem.this.selectionclicked(eventKind, e);
	    }
	    public void doubleclicked(byte eventKind, MouseEvent e) {
		DisplayItem.this.selectiondoubleclicked(eventKind, e);
	    }
	};
	addJapeMouseListener(selectionListener);
	this.indicator = indicator;
	indicator.indicate(this);
	canvas.add((Component)indicator);
    }

    public boolean selectable() {
	return indicator!=null;
    }

    public boolean getSelected() {
	return indicator!=null && selected;
    }

    public void setSelected(boolean selected) {
	if (indicator!=null && this.selected!=selected) {
	    this.selected = selected;
	    indicator.indicate(this);
	}
    }

    public void doSelectAction(boolean selected) {
	if (indicator!=null && this.selected!=selected) {
	    setSelected(selected);
	    canvas.notifySelectionChange(this);
	}
    }

    public void selectionclicked(byte eventKind, MouseEvent e) {
	canvas.claimFocus();
	if (selected) {
	    switch (eventKind) {
		case ExtendedSelection:
		case DisjointSelection:
		case ExtendedDisjointSelection:
		    canvas.doDeselectAction(this);
		    doSelectAction(false);
		    break;
		case Selection:
		    break;
		default:
		    Alert.abort("DisplayItem.selectionclicked selected event="+eventKind);
	    }
	}
	else {
	    switch (eventKind) {
		case ExtendedSelection:
		case DisjointSelection:
		case ExtendedDisjointSelection:
		    canvas.doExtendSelectAction(this);
		    doSelectAction(true);
		    break;
		case Selection:
		    canvas.doSelectAction(this);
		    doSelectAction(true);
		    break;
		default:
		    Alert.abort("DisplayItemSelectionAdapter.clicked not selected event="+eventKind);
	    }
	}
    }

    public void selectiondoubleclicked(byte eventKind, MouseEvent e) {
	switch(eventKind) {
	    case ExtendedSelection:
	    case DisjointSelection:
	    case ExtendedDisjointSelection:
		break;
	    case Selection:
		canvas.doHitAction(this);
		break;
	    default:
		Alert.abort("DisplayItem.selectiondoubleclicked event="+eventKind);
	}
    }
}
