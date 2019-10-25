/* 
        Copyright © 2003-17 Richard Bornat & Bernard Sufrin
     
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

import java.awt.Component;
import java.awt.Container;
import java.awt.event.MouseEvent;
import java.lang.reflect.Method;

@SuppressWarnings("serial")
public abstract class JapeCanvas extends ContainerWithOrigin
				 implements SelectionConstants {

    protected JapeCanvas(Container viewport, boolean scrolled) {
	super(viewport, scrolled);
	addMouseListener(new JapeMouseTextAdapter() {
	    public void clicked(byte eventKind, MouseEvent e) {
		JapeCanvas.this.claimFocus();
		JapeCanvas.this.clicked(eventKind, e);
	    }
	    // default textreleased doesn't always claim focus
	    public void textreleased(byte eventKind, boolean isClick, MouseEvent e) {
		if (isClick) {
		    JapeCanvas.this.claimFocus();
		    JapeCanvas.this.textclicked(eventKind, e);
		}
	    }
	});
    }

    protected abstract void claimFocus();
				     
    // DisplayItems (things that have an identity) get added at the front;
    // other items (lines, rects, etc.) at the back.
    public Component add(Component c) {
	if (c instanceof DisplayItem)
	    super.add(c, 0);
	else
	    super.add(c);
	return c;
    }

    // linethickness is a canvas-wide property
    protected int linethickness;

    // if linethickness changes, tell anybody who has a setlinethickness method
    public void setlinethickness(int linethickness) {
	this.linethickness = linethickness;
	int nc = child.getComponentCount();
	if (nc>0) {
	    Object [] args = new Object[] { Integer.valueOf(linethickness) };
	    for (int i=0; i<nc; i++) {
		Component c = child.getComponent(i);
		Class<? extends Component> cl = c.getClass();
		try {
		    Method m = cl.getMethod("setlinethickness", Integer.class);
		    try {
			m.invoke(c, args);
		    } catch (java.lang.IllegalAccessException e) {
			Logger.log.println("private setlinethickness in "+c);
		    } catch (Exception e) {
			Logger.log.println("setlinethickness invocation: "+c+"; => "+e);
		    } 
		} catch (java.lang.NoSuchMethodException e) { }
	    }
	}
    }

    // from it we derive the selection halo for text items
				     
    protected int getSelectionHalo() {
	return 2*linethickness;
    }

    // and the surround gap for selection items
    protected int getSelectionGap() {
	return linethickness*3/2;
    }
    
    // click on canvas kills selections
    protected void clicked(byte eventKind, MouseEvent e) {
	switch (eventKind) {
	    case Selection:
		if (getSelectionCount()!=0) {
		    killAllSelections();
		    notifySelectionChange(null);
		}
		break;
	    case ExtendedSelection:
	    case DisjointSelection:
	    case ExtendedDisjointSelection:
		break;
	    default:
	        // this appears to happen, and to cause no problems. So no more alerts.
	        // Alert.showErrorAlert("JapeCanvas.click eventKind="+eventKind);
	}
    }

    public int getSelectionCount() {
	int nc = child.getComponentCount(); // oh dear ...
	int count = 0;
	for (int i=0; i<nc; i++) {
	    Component c = child.getComponent(i);
	    if (c instanceof DisplayItem && ((DisplayItem)c).getSelected())
		count++;
	}
	return count;
    }
    
    // text click on canvas kills text selections
    protected void textclicked(byte eventKind, MouseEvent e) {
	switch (eventKind) {
	    case TextSelection:
	    case ExtendedTextSelection:
		if (getTextSelectionCount()!=0) {
		    killTextSelections(null);
		    notifyTextSelectionChange(null);
		}
		break;
	    case DisjointTextSelection:
	    case ExtendedDisjointTextSelection:
		break;
	    default:
		Alert.guiAbort("JapeCanvas.textclicked eventKind="+eventKind);
	}
    }

    // default action when selectable item is clicked - kill all selections
    protected void doSelectAction(DisplayItem item) {
	killAllSelections();
    }

    // default action when selectable item is shift-clicked - nothing
    protected void doExtendSelectAction(DisplayItem item) {
	return;
    }

    // default action when selected selectable item is shift-clicked - nothing
    protected void doDeselectAction(DisplayItem item) {
	return;
    }

    // default action when selectable item is double-clicked - nothing
    protected void doHitAction(DisplayItem item) {
	return;
    }
    
    public abstract String getSelections(String sep);

    public void killAllSelections() {
	Component[] cs = child.getComponents(); // oh dear ...
	for (int i=0; i<cs.length; i++) {
	    if (cs[i] instanceof DisplayItem) {
		((DisplayItem)cs[i]).setSelected(false);
	    }
	}
    }

    // not efficient, not in time order
    public String getPositionedContextualisedTextSelections(String sep) {
	String s = null;
	int nc = child.getComponentCount(); // oh dear ...
	for (int i=0; i<nc; i++) {
	    Component c = child.getComponent(i); // oh dear ...
	    if (c instanceof TextSelectableItem) {
		TextSelectableItem sti = (TextSelectableItem)c;
		String s1 = sti.getContextualisedTextSelections();
		if (s1!=null) {
		    s1 = sti.idX+Reply.stringSep+sti.idY+Reply.stringSep+s1;
		    if (s==null)
			s=s1;
		    else
			s=s+sep+s1;
		}
	    }
	}
	return s;
    }

    public String getSingleTextSelection() {
	if (getTextSelectionCount()==1) {
	    /* find the child with the stuff (yawn) */
	    int nc = child.getComponentCount(); // oh dear ...
	    for (int i=0; i<nc; i++) {
		Component c = child.getComponent(i); // oh dear ...
		if (c instanceof TextSelectableItem) {
		    TextSelectableItem sti = (TextSelectableItem)c;
		    String s1 = sti.getSingleTextSelection();
		    if (s1!=null)
			return s1;
		}
	    }
	}

	return null;
    }

    public void killAllTextSelections() {
	killTextSelections(null);
    }
    
    protected void killTextSelections(TextSelectableItem leave) {
	Component[] cs = child.getComponents(); // oh dear ...
	for (int i=0; i<cs.length; i++)
	    if (cs[i] instanceof TextSelectableItem && cs[i]!=leave)
		((TextSelectableItem)cs[i]).deTextSelect();
    }

    public int getTextSelectionCount() {
	int nc = child.getComponentCount(); // oh dear ...
	int count = 0;
	for (int i=0; i<nc; i++) {
	    Component c = child.getComponent(i);
	    if (c instanceof TextSelectableItem)
		count += ((TextSelectableItem)c).getTextSelectionCount();
	}
	return count;
    }

    // default is not to notify selection changes
    protected void notifyTextSelectionChange(DisplayItem item) { 
	return;
    }

    protected void notifySelectionChange(DisplayItem item) { 
	return;
    }
    
    public ProofWindow getProofWindow() {
	Container c = getParent();
	while (c!=null && !(c instanceof ProofWindow))
	    c = c.getParent();
	return (ProofWindow)c; // null if we ain't in a ProofWindow, obviously
    }
}
