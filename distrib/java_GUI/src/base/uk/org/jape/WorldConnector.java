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

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Point;

import java.awt.event.MouseEvent;

import javax.swing.JFrame;
import javax.swing.JLayeredPane;
import javax.swing.SwingUtilities;

@SuppressWarnings("serial")
public class WorldConnector extends LineItem implements SelectionConstants, WorldTarget {
    public WorldCanvas canvas;
    public final WorldItem from, to;

    private final JLayeredPane layeredPane;
    private final Container contentPane;
    
    public WorldConnector(WorldCanvas canvas, JFrame window, WorldItem from, WorldItem to) {
	super(canvas, from.idX, -from.idY, to.idX, -to.idY);
	this.canvas = canvas;
	this.from = from; this.to = to;
	this.layeredPane = window.getLayeredPane();
	this.contentPane = window.getContentPane();
	
	from.registerFrom(this); to.registerTo(this);
	
	addMouseInputListener(new JapeMouseAdapter() {
	    private boolean noticeDrag;
	    public void pressed(MouseEvent e) {
		WorldConnector.this.canvas.claimFocus();
		noticeDrag = !(e.isAltDown() || e.isShiftDown() ||
			       e.isMetaDown() || e.isControlDown());
		if (noticeDrag)
		    WorldConnector.this.pressed(e);
	    }
	    public void dragged(boolean wobbly, MouseEvent e) {
		if (wobbly && noticeDrag)
		    WorldConnector.this.dragged(e); // don't take notice of small movements
	    }
	    public void released(MouseEvent e) {
		if (noticeDrag)
		    WorldConnector.this.released(e);
	    }
	});
    }

    /* ****************************** line as drag target ****************************** */

    private static final int wobble = 3;
    
    
    public boolean contains(int x, int y) {
        Point xy = new Point(x,y);
        int width = getWidth(), height = getHeight();
        double dist = 
            // which diagonal are we?
            (x0<=x1 && y0<=y1) || (x1<=x0 && y1<=y0) ? 
                    JapeUtils.pointToLineDistance(xy, new Point(0,0), new Point(width, height)) :
                    JapeUtils.pointToLineDistance(xy, new Point(0,height), new Point(width, 0));
        return dist<(double)wobble;
    }

    private boolean draghighlight;
    private Color previousColour;

    private void setDragHighlight(boolean state) {
	if (state!=draghighlight) {
	    draghighlight = state;
	    if (draghighlight) {
		previousColour = getForeground();
		setForeground(JapePrefs.SelectionColour);
		repaint();
	    }
	    else {
		setForeground(previousColour);
		repaint();
	    }
	}
    }

    private boolean dragEnter(boolean ok) {
	setDragHighlight(ok); return ok;
    }

    private void dragExit() {
	setDragHighlight(false);
    }

    // WorldTarget
    public boolean dragEnter(byte dragKind, WorldItem w) { return dragEnter(true); }
    public void dragExit(byte dragKind, WorldItem w) { dragExit(); }

    /* ****************************** line as drop target ****************************** */

    public void drop(byte dragKind, WorldItem w, int x, int y) {
	if (draghighlight) {
	    Reply.sendCOMMAND(dragKind==MoveWorldDrag ? "moveworldtolink" : "addworldtolink",
			      w.idX, w.idY, x+getX(), -(y+getY()), from.idX, from.idY, to.idX, to.idY);
	    dragExit();
	}
	else
	    Alert.guiAbort("world drop on non-accepting line");
    }

    /* ****************************** line as drag source ****************************** */

    private int startx, starty, lastx, lasty;
    private boolean firstDrag;
    private DragWorldLine dragLine, other;
    private LineTarget over;

    private void pressed(MouseEvent e) {
	startx = e.getX(); starty = e.getY(); firstDrag = true;
    }

    private RegisteredDrag dragee;
    
    protected void dragged(MouseEvent e) {
	if (firstDrag) {
	    firstDrag = false;
	    over = null;
	    dragee = new RegisteredDrag(){
	        public void released(MouseEvent re) {
	            WorldConnector.this.released(re);
	        }
	    };
	    Jape.registerDrag(dragee);
	    Point p = SwingUtilities.convertPoint(this, e.getX(), e.getY(), layeredPane);
	    dragLine = new DragWorldLine(from, p.x, p.y, canvas.linethickness, false);
	    other = new DragWorldLine(to, p.x, p.y, canvas.linethickness, true);
	    dragLine.addFriend(other);
	    layeredPane.add(dragLine, JLayeredPane.DRAG_LAYER);
	    layeredPane.add(other, JLayeredPane.DRAG_LAYER);
	    if (DebugVars.drag_tracing)
		Logger.log.println("; dragged line at "+dragLine.activex+","+dragLine.activey);
	    dragLine.repaint();
	    setVisible(false);
	}
	else {
	    if (DebugVars.drag_tracing)
		Logger.log.print("mouse dragged to "+e.getX()+","+e.getY());
	    dragLine.moveBy(e.getX()-lastx, e.getY()-lasty);
	    if (DebugVars.drag_tracing)
		Logger.log.println("; dragged line now at "+dragLine.activex+","+dragLine.activey);
	    Point p = SwingUtilities.convertPoint(this, e.getX(), e.getY(), contentPane);
	    /* if I knew how to make this polymorphic in Java, I would */
	    Component target = contentPane.findComponentAt(p);
	    if (target instanceof ContainerWithOrigin.Child)
	        target = target.getParent();
	    if (target!=null && target instanceof LineTarget) {
	        LineTarget ltarget = (LineTarget)target;
	        if (ltarget!=over) {
	            if (over!=null) {
	                over.dragExit(this); over=null;
	            }
	            if (ltarget!=null && ltarget.dragEnter(this))
	                over = ltarget;
	        }   
	    }
	    else
	    if (over!=null) {
                over.dragExit(this); over=null;
            }
	}
	lastx = e.getX(); lasty = e.getY();
    }

    protected void released(MouseEvent e) {
        Jape.deregisterDrag(dragee);
	if (DebugVars.drag_tracing)
	    Logger.log.println("mouse released at "+e.getX()+","+e.getY()+
			       "; dragged line at "+dragLine.activex+","+dragLine.activey);
	if (over==null)
	    new Flyback(dragLine, dragLine.activex, dragLine.activey,
			SwingUtilities.convertPoint(this, startx, starty, layeredPane)) {
		protected void finishFlyback() { finishDrag(); setVisible(true); }
	    };
	else {
	    over.drop(this);
	    finishDrag();
	}
    }

    protected void finishDrag() {
	layeredPane.remove(dragLine);
	layeredPane.remove(other);
	layeredPane.repaint();
    }

    public void setVisible(boolean visible) {
	super.setVisible(visible);
	repaint();
    }
}
