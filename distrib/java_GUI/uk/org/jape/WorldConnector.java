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

import java.awt.Color;
import java.awt.Container;
import java.awt.Point;

import java.awt.event.MouseEvent;

import javax.swing.JFrame;
import javax.swing.JLayeredPane;
import javax.swing.SwingUtilities;

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
        
        addJapeMouseListener(new JapeMouseAdapter() {
            private boolean noticeDrag;
            public void pressed(MouseEvent e) {
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
        if (-wobble<=x && x<getWidth()+wobble && -wobble<=y && y<getHeight()+wobble) {
            if (x0==x1)
                return Math.abs(x-x0)<=wobble; // vertical
            else
            if (y0==y1)
                return  Math.abs(y-y0)<=wobble; // horizontal
            else {
                float a = y1-y0, b = x0-x1, c = a*x0+b*y0;
                float dx = x-(c-b*y)/a, dy = y-(c-a*x)/b;
                return  Math.abs(dx)<=wobble ||  Math.abs(dy)<=wobble ||
                        dx*dx*dy*dy/(dx*dx+dy*dy)<=wobble*wobble;
            }
        }
        else
            return false;
    }

    private boolean draghighlight;
    private Color previousColour;

    private void setDragHighlight(boolean state) {
        if (state!=draghighlight) {
            draghighlight = state;
            if (draghighlight) {
                previousColour = getForeground();
                setForeground(Preferences.SelectionColour);
                canvas.imageRepaint(); repaint();
            }
            else {
                setForeground(previousColour);
                canvas.imageRepaint(); repaint();
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
            Reply.sendCOMMAND((dragKind==MoveWorldDrag ? "moveworldtolink" : "addworldtolink")+
                              " "+w.idX+" "+w.idY+
                              " "+(x+getX())+" "+(-(y+getY()))+
                              " "+from.idX+" "+from.idY+" "+to.idX+" "+to.idY);
            dragExit();
        }
        else
            Alert.abort("world drop on non-accepting line");
    }

    /* ****************************** line as drag source ****************************** */

    private int startx, starty, lastx, lasty;
    private boolean firstDrag;
    private DragWorldLine dragLine, other;
    private Class targetClass;
    private LineTarget over;

    private void pressed(MouseEvent e) {
        startx = e.getX(); starty = e.getY(); firstDrag = true;
    }

    protected void dragged(MouseEvent e) {
        if (firstDrag) {
            firstDrag = false;
            try {
                targetClass = Class.forName("LineTarget");
            } catch (ClassNotFoundException exn) {
                Alert.abort("can't make LineTarget a Class");
            }
            over = null;
            Point p = SwingUtilities.convertPoint(this, e.getX(), e.getY(), layeredPane);
            dragLine = new DragWorldLine(from, p.x, p.y, canvas.linethickness, false);
            other = new DragWorldLine(to, p.x, p.y, canvas.linethickness, true);
            dragLine.addFriend(other);
            layeredPane.add(dragLine, JLayeredPane.DRAG_LAYER);
            layeredPane.add(other, JLayeredPane.DRAG_LAYER);
            if (drag_tracing)
                System.err.println("; dragged line at "+dragLine.activex+","+dragLine.activey);
            dragLine.repaint();
            setVisible(false);
        }
        else {
            if (drag_tracing)
                System.err.print("mouse dragged to "+e.getX()+","+e.getY());
            dragLine.moveBy(e.getX()-lastx, e.getY()-lasty);
            if (drag_tracing)
                System.err.println("; dragged line now at "+dragLine.activex+","+dragLine.activey);
            Point p = SwingUtilities.convertPoint(this, e.getX(), e.getY(), contentPane);
            LineTarget target = (LineTarget)japeserver.findTargetAt(targetClass, contentPane, p.x, p.y);
            if (target!=over) {
                if (over!=null) {
                    over.dragExit(this); over=null;
                }
                if (target!=null && target.dragEnter(this))
                    over = target;
            }
        }
        lastx = e.getX(); lasty = e.getY();
    }

    protected void released(MouseEvent e) {
        if (drag_tracing)
            System.err.println("mouse released at "+e.getX()+","+e.getY()+
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
        canvas.imageRepaint(); repaint();
    }
}
