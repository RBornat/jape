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

import java.awt.Component;
import java.awt.Container;
import java.awt.FontMetrics;

import java.awt.event.MouseEvent;

public class ProvisoCanvas extends JapeCanvas implements ProtocolConstants {

    public ProvisoCanvas(Container viewport, boolean scrolled) {
	super(viewport, scrolled);
	clear();
    }

    protected void claimFocus() {
	getProofWindow().claimProofFocus();
    }

    public String getSelections(String sep) {
	Alert.abort("ProvisoCanvas.getSelections");
	return null; // shut up compiler
    }
    
    // not efficient, not in time order
    public String getTextSelections(String sep) {
	String s = null;
	int nc = child.getComponentCount(); // oh dear ...
	for (int i=0; i<nc; i++) {
	    Component c = child.getComponent(i); // oh dear ...
	    if (c instanceof TextSelectableItem) {
		TextSelectableItem sti = (TextSelectableItem)c;
		String s1 = sti.getTextSelections();
		if (s1!=null) {
		    if (s==null)
			s=s1;
		    else
			s=s+sep+s1;
		}
	    }
	}
	return s;
    }

    public Component add(Component c) {
	Alert.abort("ProvisoCanvas.add("+c+")");
	return c; // shut up compiler
    }

    public Component add(Component c, int index) {
	Alert.abort("ProvisoCanvas.add("+c+", "+index+")");
	return c; // shut up compiler
    }

    public void remove(Component c) {
	Alert.abort("ProvisoCanvas.remove("+c+")");
    }

    public void removeAll() {
	Alert.abort("ProvisoCanvas.removeAll()");
    }

    private int initCursorValue() {
	ensureFontInfo();
	return inset+ascent;
    }
    
    public void clear() {
	super.removeAll();
	setOrigin(0,0); // maybe ...
	ensureFontInfo();
	provisoCursor = givenCursor = initCursorValue();
	showGivens();
    }

    private static int ascent=-1, descent, leading; 
    private static int inset, linestep;

    private void ensureFontInfo() {
	if (ascent==-1) {
	    FontMetrics f = JapeFont.getFontMetrics(ProvisoFontNum);
	    ascent = f.getMaxAscent(); descent = f.getMaxDescent();
	    leading = f.getLeading();
	    linestep = ascent+descent+leading;
	    inset = Math.max(2, 2*leading);
	}
    }
    
    private int provisoCursor, givenCursor;

    private void shiftLines(int cursor) {
	ensureFontInfo();
	int nc = child.getComponentCount();
	for (int i=0; i<nc; i++) {
	    TextItem t = (TextItem)child.getComponent(i);
	    if (t.getY()+ascent>=cursor) {
		t.repaint();
		t.setLocation(t.getX(), t.getY()+linestep);
		t.repaint();
	    }
	}
    }

    private void insertHeader(int cursor, String text) {
	shiftLines(cursor);
	super.add(new TextItem(this, inset, cursor, ProvisoFontNum, text));
    }

    private TextSelectableItem insertLine(int cursor, String annottext) {
	shiftLines(cursor);
	return (TextSelectableItem)
	       super.add(new TextSelectableItem(this, 3*inset, cursor, ProtocolConstants.ProvisoFontNum, annottext));
    }

    private static String provisoHeader = "Provided:",
			  givenHeader	= "Given";

    public void addProvisoLine(String annottext) {
	ensureFontInfo();
	if (provisoCursor==initCursorValue()) {
	    insertHeader(provisoCursor, provisoHeader);
	    provisoCursor += linestep; givenCursor += linestep;
	}
	insertLine(provisoCursor, annottext);
	provisoCursor += linestep; givenCursor += linestep;
    }

    MiscellaneousConstants.IntString[] givens;
    
    public void setGivens(MiscellaneousConstants.IntString[] gs) {
	givens = gs;
	showGivens();
    }

    private void mkGivenLine(TextSelectableItem tspi, final int giveni) {
	tspi.addJapeMouseListener( new JapeMouseAdapter() {
	    public void doubleclicked(MouseEvent e) {
		getProofWindow().claimProofFocus();
		Reply.sendCOMMAND("applygiven "+giveni);
	    }
	});
    }

    private void showGivens() {
	// clear the old ones
	for (int i=0; i<child.getComponentCount(); ) {
	    TextItem t = (TextItem)child.getComponent(i);
	    if (t.getY()+ascent>=provisoCursor)
		super.remove(t);
	    else
		i++;
	}
	givenCursor = provisoCursor;
	if (givens!=null && givens.length!=0) {
	    insertHeader(givenCursor, givenHeader);
	    givenCursor += linestep;
	    for (int i=0; i<givens.length; i++) {
		mkGivenLine(insertLine(givenCursor, givens[i].s), givens[i].i);
		givenCursor += linestep;
	    }
	}
    }
}
