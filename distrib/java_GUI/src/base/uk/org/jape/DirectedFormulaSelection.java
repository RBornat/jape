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

import java.awt.Graphics;

@SuppressWarnings("serial")
public class DirectedFormulaSelection extends RectSelection
		   implements SelectionConstants {

    /*	Reasons are selected by surrounding them with a box.
    Hypotheses are open at the bottom, conclusions at the top.
    If marked ambiguous (can be clicked either way) the open section is a dotted line.
    */

    public DirectedFormulaSelection(DisplayItem item) {
    super(item);
    }

    protected void paintDotted(Graphics g, int y) {
    int linethickness = canvas.linethickness;
    int dashlength = 3*linethickness;
    for (int i=0; i<right; i+=dashlength) {
	if ((i/3)%2==0) g.drawLine(i,y, Math.min(right, i+dashlength-1),y);
    }
    }

    protected void paintHooks(Graphics g, int y) {
    int linethickness = canvas.linethickness;
    int hooklength = Math.min(4*linethickness, getWidth()/6);
    if (hooklength>linethickness) {
	int lefthook = hooklength-1, righthook = right-hooklength+1;
	g.drawLine(left,y, lefthook,y);
	g.drawLine(righthook,y, right,y);
    }
    }

    private boolean ambiguous;
    private byte selectionKind;
    
    public void indicate(DisplayItem di) {
    if (di instanceof SelectableProofItem) {
	SelectableProofItem item = (SelectableProofItem)di;
	selectionKind = item.getSelectionKind();
	ambiguous = item.getAmbiguous();
    }
    super.indicate(di);
    }
    
    public void paint(Graphics g) {
    if (getSelected()) {
	if (DebugVars.paint_tracing)
	Logger.log.println("painting DirectedFormulaSelection at "+getX()+","+getY());

	setForeground(JapePrefs.SelectionColour);

	switch (selectionKind) {
	case ReasonSel: // painted as a box
	    super.paint(g);
	    break;

	case HypSel:
	    prepaint(g);
	    paintSides(g);
	    if (ambiguous)
	    paintDotted(g, top);
	    else
	    paintHorizEdge(g, top); 
	    paintHooks(g, bottom);
	    break;

	case ConcSel:
	    prepaint(g);
	    paintSides(g);
	    if (ambiguous)
	    paintDotted(g, bottom);
	    else
	    paintHorizEdge(g, bottom);
	    paintHooks(g, top);
	    break;

	default:
	    Alert.abort("DirectedFormulaSelection selectionKind="+selectionKind);
	}
    }
    }
}
