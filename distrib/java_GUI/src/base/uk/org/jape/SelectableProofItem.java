/* 
    $Id$
    
    Copyright © 2003-8 Richard Bornat & Bernard Sufrin
	
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
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.MouseEvent;

import javax.swing.JLayeredPane;
import javax.swing.SwingUtilities;

@SuppressWarnings("serial")
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
	addDragIndicator(new DragIndicator(this));
	addMouseInputListener(new JapeMouseAdapter() {
	    int dragNum;
	    public void pressed(MouseEvent e) {
		SelectableProofItem.this.canvas.claimFocus();
		dragNum = getDragNum();
		if (DebugVars.drag_tracing)
		    Logger.log.println("looking for drag "+idX+","+idY+"; got "+dragNum);
		if (dragNum!=-1)
		    SelectableProofItem.this.pressed(dragNum, e);
	    }
	    public void dragged(boolean wobbly, MouseEvent e) {
		if (dragNum!=-1 && wobbly)
		    SelectableProofItem.this.dragged(dragNum, e); // don't take notice of small movements
	    }
	    public void released(MouseEvent e) {
		if (dragNum!=-1)
		    SelectableProofItem.this.released(dragNum, e);
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
    
    private int startx, starty, lastx, lasty, offsetx, offsety;
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
    
    private void pressed(int dragNum, MouseEvent e) {
	startx = e.getX(); starty = e.getY(); firstDrag = true;
    }
    
    private RegisteredDrag dragee = null;
    
    public void dragged(final int dragNum, MouseEvent e) {
	if (firstDrag) {
	    firstDrag = false;
	    dragee = new RegisteredDrag(){
	        public void released(MouseEvent re) {
	            SelectableProofItem.this.released(dragNum, re);
	        }
	    };
	    Jape.registerDrag(dragee);
	    over = null;
	    formulaImage = new FormulaImage();
	    Point p = formulaImage.getImageLocation();
	    offsetx = getX()-p.x; offsety = getY()-p.y;
	    layeredPane.add(formulaImage, JLayeredPane.DRAG_LAYER);
	    formulaImage.setLocation(SwingUtilities.convertPoint(this, e.getX()-startx-offsetx,
							       e.getY()-starty-offsety,
							       layeredPane));
	    formulaImage.repaint();
	    ((ProofCanvas)canvas).wakeDragTargetIndicators(dragNum);
	}
	else {
	    if (DebugVars.drag_tracing)
		Logger.log.print("mouse dragged to "+e.getX()+","+e.getY());
	    int deltax = e.getX()-lastx, deltay = e.getY()-lasty;
	    formulaImage.moveBy(deltax, deltay);
	    if (DebugVars.drag_tracing)
		Logger.log.println("; dragged formula now at "+formulaImage.getX()+","+formulaImage.getY());
	}
	
	Point p = SwingUtilities.convertPoint(this, e.getX(), e.getY(), contentPane);
        /* if I knew how to make this polymorphic in Java, I would */
	Component target = contentPane.findComponentAt(p);
        if (target instanceof ContainerWithOrigin.Child)
            target = target.getParent();
	if (target!=null && target instanceof FormulaTarget) {
	    FormulaTarget ftarget = (FormulaTarget)target;
	    if (ftarget!=over) {
	        if (over!=null) {
	            over.dragExit(this); over=null;
	        }
	        if (ftarget!=null && ftarget.dragEnter(dragNum, this))
	            over = ftarget;
	    }	
	}
	else
	if (over!=null) {
            over.dragExit(this); over=null;
        } 
	lastx = e.getX(); lasty = e.getY();
    }
    
    protected void released(int dragNum, MouseEvent e) {
        Jape.deregisterDrag(dragee);
	if (DebugVars.drag_tracing)
	    Logger.log.println("mouse released at "+e.getX()+","+e.getY()+
			       "; dragged formula at "+formulaImage.getX()+","+formulaImage.getY());
	if (over==null)
	    new Flyback(formulaImage, formulaImage.getLocation(),
			SwingUtilities.convertPoint(this, -offsetx, -offsety, layeredPane)) {
		protected void finishFlyback() {
		    finishDrag();
		    ((ProofCanvas)canvas).wakeDragSourceIndicators();
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

    public int getDragNum() {
	return pointIsIn(dragSources, idX, idY);
    }
    
    public boolean isTarget(int dragNum) {
	int dropNum = pointIsIn(dragTargets, idX, idY);
	if (dropNum!=-1) {
	    /* look for source, target pair */
	    for (int pair=0; pair*2+1<dragMap.length; pair++) {
		int[] ss = dragMap[pair*2], ts = dragMap[pair*2+1];
		for (int si=0; si<ss.length; si++)
		    if (ss[si]==dragNum) {
			for (int ti=0; ti<ts.length; ti++)
			    if (ts[ti]==dropNum)
				return true; 
			break;
		    }
	    }
	}
	return false;
    }
    
    public boolean dragEnter(int dragNum, SelectableProofItem f) {
	acceptDrop = isTarget(dragNum);
	if (acceptDrop) {
	    paintTextSels = false;
	    repaint();
	}
	return acceptDrop;
    }

    public void dragExit(SelectableProofItem f) { 
	acceptDrop = false;
	paintTextSels = true;
	repaint();
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

    private static Point[][] dragSources = new Point[0][0], 
                             dragTargets = new Point[0][0];

    private static int pointIsIn(Point[][] es, int x, int y) {
	for (int i=0; i<es.length; i++) {
	    Point[] e = es[i];
	    for (int j=0; j<e.length; j++) {
		if (DebugVars.drag_tracing)
		    Logger.log.println("["+i+","+j+"]=="+e[j].x+","+e[j].y);
		if (e[j].x==x && e[j].y==y)
		    return i;
	    }
	}
	return -1;
    }

    private static int[][] dragMap = new int[0][0];

    public static void setDragSources(Point[][] dragSources) {
	SelectableProofItem.dragSources = dragSources;
    }

    public static void setDragTargets(Point[][] dragTargets) {
	SelectableProofItem.dragTargets = dragTargets;
    }

    public static void setDragMap(int[][] dragMap) {
	SelectableProofItem.dragMap = dragMap;
    }
}
