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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;

import java.awt.event.MouseEvent;

import java.awt.geom.Ellipse2D;

import java.awt.image.BufferedImage;

import java.util.Vector;

import javax.swing.JFrame;
import javax.swing.JLayeredPane;
import javax.swing.SwingUtilities;

public class WorldItem extends DisplayItem implements DebugConstants, MiscellaneousConstants,
                                                      SelectionConstants,
                                                      TileTarget, WorldTarget {

    protected WorldCanvas canvas;
    protected JFrame window;
    protected JLayeredPane layeredPane;
    protected Container contentPane;
    protected SelectionRing selectionRing;
    protected Ellipse2D.Float outline;

    private final int x0, y0, radius, labelgap;
    private int labelx;
    private Vector labelv = new Vector(),
                   fromv  = new Vector(),
                   tov    = new Vector();
    
    public WorldItem(WorldCanvas canvas, JFrame window, int x, int y) {
        super(x, y);
        this.canvas = canvas; this.window = window;
        this.layeredPane = window.getLayeredPane();
        this.contentPane = window.getContentPane();
        this.x0 = x; this.y0 = -y;
        this.radius = canvas.worldRadius();
        setBounds(x0-radius, y0-radius, 2*radius, 2*radius);

        selectionRing = new SelectionRing(x0, y0, radius+2*canvas.linethickness);
        canvas.add(selectionRing);

        outline = new Ellipse2D.Float(0, 0, getWidth(), getHeight());
        if (geometry_tracing)
            System.err.println("world bounds are "+getBounds()+"; outline is "+outline.getBounds2D());

        setForeground(Preferences.WorldColour);

        labelx = selectionRing.getX()+selectionRing.getWidth()+canvas.linethickness;
        labelgap = 4*canvas.linethickness;

        addJapeMouseListener(new JapeMouseAdapter() {
            private byte clickKind, dragKind;
            public void clicked(MouseEvent e) {
                if (clickKind==WorldClick)
                    Reply.sendCOMMAND("worldselect "+idX+" "+idY);
            }
            public void pressed(MouseEvent e) {
                dragKind = LocalSettings.mousePressWorldItemMeans(e);
                clickKind = LocalSettings.mouseClickWorldItemMeans(e);
                WorldItem.this.pressed(dragKind, e);
            }
            public void dragged(boolean wobbly, MouseEvent e) {
                if (wobbly)
                    WorldItem.this.dragged(dragKind, e); // don't take notice of small movements
            }
            public void released(MouseEvent e) {
                WorldItem.this.released(dragKind, e);
            }
        });
    }

    public void registerTo(WorldConnector wc) {
        for (int i=0; i<tov.size(); i++) {
            WorldConnector wc1 = (WorldConnector)tov.get(i);
            if (wc1.from==wc.from)
                return;
        }
        tov.add(wc);
    }

    public void registerFrom(WorldConnector wc) {
        for (int i=0; i<fromv.size(); i++) {
            WorldConnector wc1 = (WorldConnector)fromv.get(i);
            if (wc1.to==wc.to)
                return;
        }
        fromv.add(wc);
    }

    public void addlabel(String s) {
        WorldLabel label = new WorldLabel(canvas, window, this, labelx, y0, s);
        canvas.add(label);
        labelv.add(label);
        labelx += label.getWidth()+labelgap;
    }
    
    public void select(boolean selected) {
        selectionRing.select(selected);
    }

    public void paint(Graphics g) {
        if (paint_tracing)
            System.err.println("painting WorldItem at "+getX()+","+getY());
        g.setColor(getForeground());
        if (g instanceof Graphics2D) {
            if (antialias_tracing) {
                System.err.print("blob hints "+((Graphics2D)g).getRenderingHints());
                if (japeserver.onMacOS)
                    System.err.println(" hwaccel "+System.getProperty("com.apple.hwaccel"));
                else
                    System.err.println();
            }
            ((Graphics2D)g).fill(outline);
        }
        else
            g.fillOval(0, 0, getWidth(), getHeight());
    }
    
    protected class SelectionRing extends CircleItem {
        private boolean selected;

        SelectionRing(int x, int y, int radius) {
            super(WorldItem.this.canvas, x, y, radius);
            setForeground(Preferences.SelectionColour);
            if (geometry_tracing)
                System.err.println("ring bounds are "+getBounds()+
                                   "; outline is "+this.outline.getBounds2D());
        }

        public void paint(Graphics g) {
            if (selected) super.paint(g);
        }

        public void select(boolean selected) {
            this.selected = selected;
            WorldItem.this.canvas.imageRepaint(); repaint();
        }
    }

    private boolean draghighlight;
    Color oldForeground;

    private void setDragHighlight(boolean state) {
        if (state && !draghighlight) {
            draghighlight = true;
            oldForeground = getForeground();
            setForeground(Preferences.SelectionColour);
            if (worldpaint_tracing)
                System.err.println("highlighting world");
            canvas.imageRepaint(); repaint();
        }
        else
            if (!state && draghighlight) {
                draghighlight = false;
                setForeground(oldForeground);
                if (worldpaint_tracing)
                    System.err.println("de-highlighting world");
                canvas.imageRepaint(); repaint();
            }
    }

    /* ****************************** world as drag target ****************************** */

    private boolean acceptDrag(Object o) {
        if (o instanceof Tile) { // only have the label once, thankyou
            String text = ((Tile)o).text;
            for (int i=0; i<labelv.size(); i++)
                if (text.equals(((WorldLabel)labelv.get(i)).text))
                    return false;
            return true;
        }
        else
        if (o instanceof WorldItem)
            return o!=this;
        else
            return false;
    }

    public boolean dragEnter(Object o) { 
        if (acceptDrag(o)) { 
            setDragHighlight(true); return true;
        }
        else
            return false;
    }

    public void dragExit() {
        setDragHighlight(false);
    }

    /* ****************************** world as drop target ****************************** */

    public void drop(Tile t) {
        if (draghighlight) {
            Reply.sendCOMMAND("addworldlabel "+idX+" "+idY+" "+"\""+t.text+"\"");
            setDragHighlight(false);
        }
        else
            Alert.abort("tile drop on non-accepting world");
    }

    public void drop(byte dragKind, WorldItem w, int x, int y) {
        if (draghighlight)
            Reply.sendCOMMAND((dragKind==MoveWorldDrag ? "moveworld" : "addworld")+
                            " "+w.idX+" "+w.idY+" "+idX+" "+idY);
        else
            Alert.abort("world drop on non-accepting world");
    }

    /* ****************************** world as drag source ****************************** */

    private int startx, starty, lastx, lasty, offsetx, offsety, centreoffsetx, centreoffsety;
    private boolean firstDrag;

    private void pressed(byte dragKind, MouseEvent e) {
        startx = e.getX(); starty = e.getY(); firstDrag = true;
    }

    protected class WorldImage extends DragComponent {
        public final byte dragKind;
        public WorldImage(byte dragKind) {
            super(Transparent); this.dragKind = dragKind;
            include(WorldItem.this);
            if (selectionRing.selected && dragKind==MoveWorldDrag)
                include(selectionRing);
            for (int i=0; i<labelv.size(); i++)
                include((WorldLabel)labelv.get(i));
            fixImage();
        }
        public void moveTo(int x, int y) {
            super.moveTo(x,y);
            dragLines.wakeup();
        }
    }

    private WorldImage worldImage;
    private WorldTarget over;
    private Class targetClass;

    private void setDrageesVisible(boolean state) {
        this.setVisible(state); selectionRing.setVisible(state);
        for (int i=0; i<labelv.size(); i++)
            ((WorldLabel)labelv.get(i)).setVisible(state);
        for (int i=0; i<fromv.size(); i++)
            ((WorldConnector)fromv.get(i)).setVisible(state);
        for (int i=0; i<tov.size(); i++)
            ((WorldConnector)tov.get(i)).setVisible(state);
        canvas.forcerepaint();
    }

    public Point dragCentre() {
        return SwingUtilities.convertPoint(this, radius, radius, layeredPane);
    }

    class DragLine extends LineComponent {
        private final int endx, endy;
        public DragLine(WorldItem w) {
            this(w.dragCentre());
        }
        public DragLine(Point p) {
            super(worldImage.getX()+offsetx+radius, worldImage.getY()+offsety+radius,
                  p.x, p.y, canvas.linethickness);
            this.endx = p.x; this.endy = p.y;
        }
        public void wakeup() {
            repaint();
            resetLine(worldImage.getX()+offsetx+radius, worldImage.getY()+offsety+radius,
                      endx, endy, canvas.linethickness);
            repaint();
        }
    }

    class DragLines {
        private Vector lines = new Vector();
        public void addLine(WorldItem w) {
            DragLine dl = new DragLine(w);
            lines.add(dl);
            layeredPane.add(dl);
            dl.repaint();
        }
        public void wakeup() {
            for (int i=0; i<lines.size(); i++)
                ((DragLine)lines.get(i)).wakeup();
        }
        public void tidy() {
            for (int i=0; i<lines.size(); i++)
                layeredPane.remove((Component)lines.get(i));
            lines.removeAllElements();
        }
    }
    
    private DragLines dragLines;
    
    public void dragged(byte dragKind, MouseEvent e) {
        if (firstDrag) {
            firstDrag = false;
            try {
                targetClass = Class.forName("WorldTarget");
            } catch (ClassNotFoundException exn) {
                Alert.abort("can't make WorldTarget a Class");
            }
            over = null;
            worldImage = new WorldImage(dragKind);
            Point p = worldImage.getImageLocation();
            offsetx = getX()-p.x; offsety = getY()-p.y;
            centreoffsetx = offsetx+radius; centreoffsety = offsety+radius;
            layeredPane.add(worldImage, JLayeredPane.DRAG_LAYER);
            worldImage.setLocation(
                SwingUtilities.convertPoint(this, e.getX()-startx-offsetx,
                                                  e.getY()-starty-offsety,
                                                  layeredPane));
            worldImage.repaint();
            dragLines = new DragLines();
            switch (dragKind) {
                case MoveWorldDrag:
                    setDrageesVisible(false);
                    for (int i=0; i<fromv.size(); i++)
                        dragLines.addLine(((WorldConnector)fromv.get(i)).to);
                    for (int i=0; i<tov.size(); i++)
                        dragLines.addLine(((WorldConnector)tov.get(i)).from);
                    break;
                case NewWorldDrag:
                    dragLines.addLine(this);
                    break;
                default:
                    Alert.abort("WorldItem.dragged dragKind="+dragKind);
            }
            if (dragKind==MoveWorldDrag && canvas.worldCount()==1)
                canvas.wasteBin.setEnabled(false);
        }
        else {
            if (drag_tracing)
                System.err.print("mouse dragged to "+e.getX()+","+e.getY());

            worldImage.moveTo(worldImage.getX()+(e.getX()-lastx),
                              worldImage.getY()+(e.getY()-lasty));
            if (drag_tracing)
                System.err.println("; dragged world now at "+worldImage.getX()+","+worldImage.getY());
        }
            
        Point p = SwingUtilities.convertPoint(this, e.getX(), e.getY(), contentPane);
        WorldTarget target = (WorldTarget)japeserver.findTargetAt(targetClass, contentPane, p.x, p.y);
        if (target!=over) {
            if (over!=null) {
                over.dragExit(); over=null;
            }
            if (target!=null && target.dragEnter(WorldItem.this))
                over = target;
        }
        lastx = e.getX(); lasty = e.getY();
    }

    protected void released(final byte dragKind, MouseEvent e) {
        if (drag_tracing)
            System.err.println("mouse released at "+e.getX()+","+e.getY()+
                               "; dragged world at "+worldImage.getX()+","+worldImage.getY());
        if (over==null)
            new Flyback(worldImage, worldImage.getLocation(),
                        SwingUtilities.convertPoint(this, -offsetx, -offsety, layeredPane)) {
                protected void finishFlyback() {
                    finishDrag();
                    if (dragKind==MoveWorldDrag)
                        setDrageesVisible(true);
                }
            };
        else {
            Point p = SwingUtilities.convertPoint(layeredPane, worldImage.getX()+offsetx+radius,
                                                  worldImage.getY()+offsety+radius, (Component)over);
            finishDrag();
            over.drop(dragKind, this, p.x, p.y);
        }
    }

    protected void finishDrag() {
        layeredPane.remove(worldImage);
        dragLines.tidy();
        layeredPane.repaint();
        canvas.wasteBin.setEnabled(true);
    }
}