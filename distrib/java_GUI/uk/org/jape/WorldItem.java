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
import javax.swing.SwingUtilities;

public class WorldItem extends DisplayItem implements DebugConstants, SelectionConstants,
                                                      TileTarget /*,
                                                      DragSourceListener, DragGestureListener,
                                                      DropTargetListener */{

    protected WorldCanvas canvas;
    protected JFrame window;
    protected SelectionRing selectionRing;
    protected Ellipse2D.Float outline;

    private final int x0, y0, labelgap;
    private int labelx;
    private Vector labelv = new Vector();

    /*public static DataFlavor worldFlavor;
    private DragSource dragSource;*/

    public WorldItem(WorldCanvas canvas, JFrame window, int x, int y) {
        super(x, y);
        this.canvas = canvas; this.window = window;
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

        /* addJapeMouseListener(new JapeMouseAdapter() {
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
        }); */
        
        /*if (worldFlavor==null) {
            try {
                worldFlavor = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType+
                                            "; class="+this.getClass().getName());
            } catch (ClassNotFoundException e) {
                Alert.abort("can't create worldFlavor");
            }
        }

        dragSource = new DragSource();
        dragSource.createDefaultDragGestureRecognizer(this, DnDConstants.ACTION_MOVE, this);*/
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
                System.err.println("ring bounds are "+getBounds()+"; outline is "+this.outline.getBounds2D());
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

    /*protected class WorldTransferable implements Transferable {
        public Object getTransferData(DataFlavor flavor) {
            return WorldItem.this;
        }
        public DataFlavor[] getTransferDataFlavors() {
            return new DataFlavor[]{ worldFlavor };
        }
        public boolean isDataFlavorSupported(DataFlavor flavor) {
            return flavor==worldFlavor;
        }
    }

    private BufferedImage image;
    private Rectangle imagebounds;
    private Point from;

    private void paintStuff(Component c, Graphics imageGraphics) {
        int deltax = c.getX()-imagebounds.x, deltay = c.getY()-imagebounds.y;
        imageGraphics.translate(deltax, deltay);
        if (dragimage_tracing)
            System.err.println("painting "+c+" @ ("+deltax+","+deltay+")");
        c.paint(imageGraphics);
        imageGraphics.translate(-deltax, -deltay);
    }
    
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