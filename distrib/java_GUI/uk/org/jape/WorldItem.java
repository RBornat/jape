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
import java.awt.Graphics;
import java.awt.Graphics2D;

import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;

import java.awt.geom.Ellipse2D;

public class WorldItem extends DisplayItem implements DebugConstants, DropTargetListener {

    protected WorldCanvas canvas;
    protected SelectionRing selectionRing;
    protected Ellipse2D.Float outline;

    private final int x0, y0, labelgap;
    private int labelx;

    public WorldItem(WorldCanvas canvas, int x, int y) {
        super(x, y);
        this.canvas = canvas;
        x0 = x; y0 = -y-2*canvas.worldRadius();
        setBounds(x0-canvas.worldRadius(), y0-canvas.worldRadius(),
                  2*canvas.worldRadius(), 2*canvas.worldRadius());
        selectionRing = new SelectionRing(x0, y0, canvas.worldRadius()+2*canvas.linethickness);
        canvas.add(selectionRing);
        outline = new Ellipse2D.Float(0, 0, getWidth(), getHeight());
        setEnabled(true); // I think
        setDropTarget(new DropTarget(this, this));
        setForeground(Preferences.WorldColour);
        labelx = selectionRing.getX()+selectionRing.getWidth()+canvas.linethickness;
        labelgap = 4*canvas.linethickness;
    }

    public void addlabel(String s) {
        TextItem t = canvas.addLabelItem(labelx, y0, s);
        labelx += t.getWidth()+labelgap;
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
        }

        public void paint(Graphics g) {
            if (selected) super.paint(g);
        }

        public void select(boolean selected) {
            this.selected = selected; repaint();
        }
    }

    // methods for when we are a drag target
    private boolean draghighlight;
    Color oldForeground;
    
    private void setdraghighlight(boolean state) {
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

    // Called when a drag operation has encountered the DropTarget.
    public void dragEnter(DropTargetDragEvent dtde) {
        if (dtde.isDataFlavorSupported(Tile.tileFlavor) &&
            // dtde.isLocalTransfer() && -- why can't we do this?
            dtde.getDropAction()==DnDConstants.ACTION_COPY) {
            dtde.acceptDrag(DnDConstants.ACTION_COPY);
            setdraghighlight(true);
        }
    }

    // The drag operation has departed the DropTarget without dropping.
    public void dragExit(DropTargetEvent dte) {
        setdraghighlight(false);
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
            setdraghighlight(false);
        }
    }

    // Called if the user has modified the current drop gesture
    public void dropActionChanged(DropTargetDragEvent dtde) {
    }
}