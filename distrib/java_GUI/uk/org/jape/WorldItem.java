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
import java.awt.Component;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;

/*import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;

import java.awt.dnd.DragSource;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;*/

import java.awt.event.MouseEvent;

import java.awt.geom.Ellipse2D;

import java.awt.image.BufferedImage;

import java.util.Vector;

import javax.swing.JFrame;
import javax.swing.JLayeredPane;
import javax.swing.SwingUtilities;

public class WorldItem extends DisplayItem implements DebugConstants, MiscellaneousConstants,
                                                      SelectionConstants,
                                                      TileTarget /*,
                                                      DragSourceListener, DragGestureListener,
                                                      DropTargetListener */{

    protected WorldCanvas canvas;
    protected JLayeredPane layeredPane;
    protected Container contentPane;
    protected SelectionRing selectionRing;
    protected Ellipse2D.Float outline;

    private final int x0, y0, labelgap;
    private int labelx;
    private Vector labelv = new Vector();

    /*public static DataFlavor worldFlavor;
    private DragSource dragSource;*/

    public WorldItem(WorldCanvas canvas, JFrame window, int x, int y) {
        super(x, y);
        this.canvas = canvas;
        this.layeredPane = window.getLayeredPane(); this.contentPane = window.getContentPane();
        x0 = x; y0 = -y;
        setBounds(x0-canvas.worldRadius(), y0-canvas.worldRadius(),
                  2*canvas.worldRadius(), 2*canvas.worldRadius());

        selectionRing = new SelectionRing(x0, y0, canvas.worldRadius()+2*canvas.linethickness);
        canvas.add(selectionRing);

        outline = new Ellipse2D.Float(0, 0, getWidth(), getHeight());
        if (geometry_tracing)
            System.err.println("world bounds are "+getBounds()+"; outline is "+outline.getBounds2D());

       /* setEnabled(true); // I think this is necessary for dragTarget behaviour
        setDropTarget(new DropTarget(this, this));*/

        setForeground(Preferences.WorldColour);

        labelx = selectionRing.getX()+selectionRing.getWidth()+canvas.linethickness;
        labelgap = 4*canvas.linethickness;

        addJapeMouseListener(new JapeMouseAdapter() {
            private byte dragKind;
            public void pressed(MouseEvent e) {
                dragKind = LocalSettings.mouseDownWorldItemMeans(e);
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

    private boolean dragImageUpdateNeeded = true;

    public void addlabel(String s) {
        TextItem t = canvas.addLabelItem(labelx, y0, s);
        labelv.add(t);
        labelx += t.getWidth()+labelgap;
        dragImageUpdateNeeded = true;
    }
    
    public void select(boolean selected) {
        selectionRing.select(selected);
        dragImageUpdateNeeded = true;
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
            this.selected = selected; repaint();
        }
    }

    private boolean draghighlight;
    Color oldForeground;

    private void setDragHighlight(boolean state) {
        if (state && !draghighlight) {
            draghighlight = true;
            oldForeground = getForeground();
            setForeground(Preferences.SelectionColour);
            if (antialias_tracing)
                System.err.println("highlighting world");
            canvas.imageRepaint(); repaint();
        }
        else
            if (!state && draghighlight) {
                draghighlight = false;
                setForeground(oldForeground);
                if (antialias_tracing)
                    System.err.println("de-highlighting world");
                canvas.imageRepaint(); repaint();
            }
    }

    /* ****************************** world as drag target ****************************** */

    public void dragEnter() {
        setDragHighlight(true);
    }

    public void dragExit() {
        setDragHighlight(false);
    }

    /* ****************************** world as tile drag target ****************************** */

    public void drop(Tile t) {
        Reply.sendCOMMAND("addworldlabel "+idX+" "+idY+" "+"\""+t.text+"\"");
        setDragHighlight(false);
    }
    
    /* ****************************** world as drag source ****************************** */

    public void hitCanvas(WorldCanvas canvas, int x, int y) {
        Reply.sendCOMMAND((dragKind==SimpleDrag ? "moveworld" : "addworld")+" "+idX+" "+idY+" "+x+" "+(-y));
    }
    
    private int startx, starty, lastx, lasty, offsetx, offsety;
    private boolean firstDrag;

    public void pressed(byte dragKind, MouseEvent e) {
        startx = e.getX(); starty = e.getY(); firstDrag = true;
    }

    protected class WorldImage extends DragComponent {
        public WorldImage() {
            super(Transparent);
            include(WorldItem.this);
            if (selectionRing.selected)
                include(selectionRing);
            for (int i=0; i<labelv.size(); i++)
                include((TextItem)labelv.get(i));
            fixImage();
        }
    }

    private WorldImage worldImage;
    private WorldTarget over;
    private Class targetClass;

    private void setDrageesVisible(boolean state) {
        this.setVisible(state); selectionRing.setVisible(state);
        for (int i=0; i<labelv.size(); i++)
            ((TextItem)labelv.get(i)).setVisible(state);
        canvas.forcerepaint();
    }
    
    public void dragged(byte dragKind, MouseEvent e) {
        if (firstDrag) {
            firstDrag = false;
            try {
                targetClass = Class.forName("WorldTarget");
            } catch (ClassNotFoundException exn) {
                Alert.abort("can't make WorldTarget a Class");
            }
            over = null;
            if (worldImage==null || dragImageUpdateNeeded) {
                worldImage = new WorldImage();
                dragImageUpdateNeeded = false;
            }
            if (dragKind==SimpleDrag)
                setDrageesVisible(false);
            Point p = worldImage.getImageLocation();
            offsetx = getX()-p.x; offsety = getY()-p.y;
            layeredPane.add(worldImage, JLayeredPane.DRAG_LAYER);
            worldImage.setLocation(
                    SwingUtilities.convertPoint(this, e.getX()-startx-offsetx, e.getY()-starty-offsety,
                                                layeredPane));
            worldImage.repaint();
        }
        else {
            if (drag_tracing)
                System.err.print("mouse dragged to "+e.getX()+","+e.getY());
            worldImage.repaint();
            worldImage.setLocation(worldImage.getX()+(e.getX()-lastx),
                                   worldImage.getY()+(e.getY()-lasty));
            if (drag_tracing)
                System.err.println("; dragged world now at "+worldImage.getX()+","+worldImage.getY());
            worldImage.repaint();
            Point p = SwingUtilities.convertPoint(this, e.getX(), e.getY(), contentPane);
            WorldTarget target = (WorldTarget)japeserver.findTargetAt(targetClass, contentPane, p.x, p.y);
            if (target!=over) {
                if (over!=null)
                    over.dragExit();
                if (target!=null)
                    target.dragEnter();
                over = target;
            }
        }
        lastx = e.getX(); lasty = e.getY();
    }

    private byte dragKind;
    
    protected void released(final byte dragKind, MouseEvent e) {
        if (drag_tracing)
            System.err.println("mouse released at "+e.getX()+","+e.getY()+
                               "; dragged world at "+worldImage.getX()+","+worldImage.getY());
        if (over==null)
            new Flyback(worldImage, worldImage.getLocation(),
                        SwingUtilities.convertPoint(this, -offsetx, -offsety, layeredPane)) {
                protected void finishFlyback() {
                    finishDrag();
                    if (dragKind==SimpleDrag)
                        setDrageesVisible(true);
                }
            };
        else {
            int radius = canvas.worldRadius();
            Point p = SwingUtilities.convertPoint(layeredPane, worldImage.getX()+offsetx+radius,
                                                  worldImage.getY()+offsety+radius, (Component)over);
            finishDrag();
            this.dragKind = dragKind;
            over.drop(this, p.x, p.y);
        }
    }

    protected void finishDrag() {
        layeredPane.remove(worldImage);
        layeredPane.repaint();
    }

    /*
    public void dragGestureRecognized(DragGestureEvent event) {
        from = event.getDragOrigin();
        if (dragimage_tracing)
            System.err.println("dragging world image; dragOrigin="+from);
        if (image==null || dragImageUpdateNeeded) {
            // find bounding rectangle of drawable stuff
            imagebounds = getBounds();
            if (selectionRing.selected)
                imagebounds.add(selectionRing.getBounds());
            for (int i=0; i<labelv.size(); i++)
                imagebounds.add(((TextItem)labelv.get(i)).getBounds());
            int width = imagebounds.width, height = imagebounds.height;
            image = (BufferedImage)createImage(width, height);
            Graphics imageGraphics = image.createGraphics();
            imageGraphics.setClip(0, 0, width, height);
            imageGraphics.setColor(Preferences.ProofBackgroundColour);
            imageGraphics.fillRect(0, 0, width, height);
            paintStuff(this, imageGraphics);
            paintStuff(selectionRing, imageGraphics);
            for (int i=0; i<labelv.size(); i++)
                paintStuff((TextItem)labelv.get(i), imageGraphics);
            imageGraphics.dispose();
            dragImageUpdateNeeded = false;
        }

        Point offset = new Point(-from.x-(getX()-imagebounds.x), -from.y-(getY()-imagebounds.y));
        if (dragimage_tracing)
            System.err.println("from="+from+"; me="+this+"; imagebounds="+imagebounds+"; offset="+offset);
        if (japeserver.onMacOS) {
            from.x = -offset.x; from.y = -offset.y; // this seems to work ... 
        }
        
        dragSource.startDrag(event, DragSource.DefaultMoveDrop, image, offset, new WorldTransferable(), this);
    }

    public void dragEnter(DragSourceDragEvent event) { }

    public void dragExit(DragSourceEvent event) { }

    public void dragOver(DragSourceDragEvent event) { }

    public void dropActionChanged(DragSourceDragEvent event) { }

    public void dragDropEnd (DragSourceDropEvent event) {
        if (dragimage_tracing)
            System.err.println("world drag "+event.getDropSuccess());
    }

    public void draggedTo(Point to) {
        if (dragimage_tracing)
            System.err.println("draggedTo from="+from+"; to="+to+"; ("+idX+","+idY+"); ("+
                               (idX+to.x-from.x)+","+(idY-(to.y-from.y))+")");
        Reply.sendCOMMAND("moveworld "+idX+" "+idY+" "+(idX+to.x-from.x)+" "+(idY-(to.y-from.y)));
    }*/

    /* ****************************** world as drag target ****************************** */
    
   /* 

    // Called when a drag operation has encountered the DropTarget.
    public void dragEnter(DropTargetDragEvent dtde) {
        if (dtde.isDataFlavorSupported(Tile.tileFlavor) &&
            // dtde.isLocalTransfer() && -- why can't we do this?
            dtde.getDropAction()==DnDConstants.ACTION_COPY) {
            dtde.acceptDrag(DnDConstants.ACTION_COPY);
            setDragHighlight(true);
        }
    }

    // The drag operation has departed the DropTarget without dropping.
    public void dragExit(DropTargetEvent dte) {
        setDragHighlight(false);
    }

    // Called when a drag operation is ongoing on the DropTarget.
    public void dragOver(DropTargetDragEvent dtde) {
    }

    // The drag operation has terminated with a drop on this DropTarget.
    public void drop(DropTargetDropEvent dtde) {
        if (draghighlight) {
            dtde.acceptDrop(DnDConstants.ACTION_COPY);
            String label;
            try {
                label = ((Tile)dtde.getTransferable().
                                    getTransferData(Tile.tileFlavor)).text;
            } catch (Exception e) {
                Alert.abort("drop onto world not tileFlavor");
                label = ""; // shut up compiler
            }
            Reply.sendCOMMAND("addworldlabel "+idX+" "+idY+" "+"\""+label+"\"");
            dtde.dropComplete(true);
            setDragHighlight(false);
        }
    }

    // Called if the user has modified the current drop gesture
    public void dropActionChanged(DropTargetDragEvent dtde) {
    }*/
}