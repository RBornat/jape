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
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.MouseEvent;
import java.awt.Point;
import java.awt.Rectangle;

import javax.swing.JLayeredPane;
import javax.swing.SwingUtilities;

public	class SelectableProofItem extends TextSelectableItem
			       implements ProtocolConstants, MiscellaneousConstants, 
                                          FormulaTarget {
    private final boolean ambiguous;
    private byte selectionKind;
    private Container layeredPane;
    private Container contentPane;

    public SelectableProofItem(ProofCanvas canvas, int x, int y, byte selectionKind, boolean ambiguous,
			       AnnotatedTextComponent[] components) {
	super(canvas,x,y,components);
	this.selectionKind = selectionKind; // ReasonSel, HypSel, ConcSel
	this.ambiguous = ambiguous;
	this.layeredPane = canvas.window.getLayeredPane(); 
	this.contentPane = canvas.window.getContentPane();
	addSelectionIndicator(
	    canvas.proofStyle==BoxStyle ? new DirectedFormulaSelection(this) :
					  new RectSelection(this));
	addJapeMouseListener(new JapeMouseAdapter() {
	    boolean canDrag;
	    public void pressed(MouseEvent e) {
		SelectableProofItem.this.canvas.claimFocus();
		canDrag = JapeUtils.isIn(getprinttext(), dragSources);
		if (canDrag)
		    SelectableProofItem.this.pressed(e);
	    }
	    public void dragged(boolean wobbly, MouseEvent e) {
		if (canDrag && wobbly)
		    SelectableProofItem.this.dragged(e); // don't take notice of small movements
	    }
	    public void released(MouseEvent e) {
		if (canDrag)
		    SelectableProofItem.this.released(e);
	    }
	});
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
    
    /* ******************************** formula as drag source ************************************* */
    
    private int startx, starty, lastx, lasty, offsetx, offsety, centreoffsetx, centreoffsety;
    private boolean firstDrag;

    protected class FormulaImage extends DragImage {
	public FormulaImage() {
	    super(Transparent); 
	    include(SelectableProofItem.this);
	    fixImage();
	}
    }
    
    private FormulaImage formulaImage;
    private FormulaTarget over;
    private Class targetClass;

    private void pressed(MouseEvent e) {
	startx = e.getX(); starty = e.getY(); firstDrag = true;
    }
    
    public void dragged(MouseEvent e) {
	if (firstDrag) {
	    firstDrag = false;
	    targetClass = FormulaTarget.class;
	    over = null;
	    formulaImage = new FormulaImage();
	    Point p = formulaImage.getImageLocation();
	    offsetx = getX()-p.x; offsety = getY()-p.y;
	    layeredPane.add(formulaImage, JLayeredPane.DRAG_LAYER);
	    formulaImage.setLocation(SwingUtilities.convertPoint(this, e.getX()-startx-offsetx,
							       e.getY()-starty-offsety,
							       layeredPane));
	    formulaImage.repaint();
	    Reply.send("DRAGQ "+idX+" "+idY); /* ask engine for targets */
	}
	else {
	    if (drag_tracing)
		Logger.log.print("mouse dragged to "+e.getX()+","+e.getY());
	    int deltax = e.getX()-lastx, deltay = e.getY()-lasty;
	    formulaImage.moveBy(deltax, deltay);
	    if (drag_tracing)
		Logger.log.println("; dragged formula now at "+formulaImage.getX()+","+formulaImage.getY());
	}
	
	Point p = SwingUtilities.convertPoint(this, e.getX(), e.getY(), contentPane);
	FormulaTarget target = (FormulaTarget)JapeUtils. findTargetAt(targetClass, contentPane, p.x, p.y);
	if (target!=over) {
	    if (over!=null) {
		over.dragExit(SelectableProofItem.this); over=null;
	    }
	    if (target!=null && target.dragEnter(SelectableProofItem.this))
		over = target;
	}
	lastx = e.getX(); lasty = e.getY();
    }
    
    protected void released(MouseEvent e) {
	if (drag_tracing)
	    Logger.log.println("mouse released at "+e.getX()+","+e.getY()+
			       "; dragged formula at "+formulaImage.getX()+","+formulaImage.getY());
	if (over==null)
	    new Flyback(formulaImage, formulaImage.getLocation(),
			SwingUtilities.convertPoint(this, -offsetx, -offsety, layeredPane)) {
		protected void finishFlyback() {
		    finishDrag();
		}
	    };
	else {
	    Point p = SwingUtilities.convertPoint(layeredPane, formulaImage.getX()+offsetx,
						  formulaImage.getY()+offsety, (Component)over);
	    finishDrag();
	    over.drop(this, p.x, p.y);
	}
    }
    
    protected void finishDrag() {
	layeredPane.remove(formulaImage);
    }
    
    /* ******************************** formula as drag target ************************************* */

    boolean acceptDrop = false;

    public boolean dragEnter(SelectableProofItem f) {
	acceptDrop=JapeUtils.isIn(getprinttext(), dropTargets);
	if (acceptDrop) {
	    paintTextSels = false;
	    canvas.repaint();
	}
	return acceptDrop;
    }

    public void dragExit(SelectableProofItem f) { 
	acceptDrop = false;
	paintTextSels = true;
	canvas.repaint();
	return; 
    }

    public void drop(SelectableProofItem f, int x, int y) {
	if (acceptDrop) {
	    acceptDrop = false;
	    paintTextSels = true;
	    Reply.send("DROP "+f.idX+" "+f.idY+" "+idX+" "+idY);
	}
	else
	    Alert.abort("formula drop on non-accepting world");
    }

    public void paint(Graphics g) {
	if (isEnabled() && acceptDrop) {
	    if (DebugVars.paint_tracing)
		Logger.log.println("highlighting SelectableProofItem "+
				   getX()+","+getY()+" "+
				   getWidth()+","+getHeight());
	    g.setColor(JapePrefs.FormulaDragHighlightColour);
	    g.fillRect(0, 0, getWidth(), getHeight());
	}
	super.paint(g);
    }

private static String[] dragSources = new String[0], dropTargets = new String[0];

    public static void setDragSources(String[] dragSources) {
	SelectableProofItem.dragSources = dragSources;
    }

    public static void setDropTargets(String[] dropTargets) {
	SelectableProofItem.dropTargets = dropTargets;
    }
}
