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

import java.awt.AlphaComposite;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;

/*import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;

import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;*/

import java.awt.event.MouseEvent;

import java.awt.image.BufferedImage;

import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.SwingUtilities;

import javax.swing.border.Border;
import javax.swing.BorderFactory;

public class Tile extends JLabel implements DebugConstants/*,
                                            DragSourceListener, DragGestureListener*/ {
    final String text;
    private Container layeredPane;
    /*private DragSource dragSource;
    public static DataFlavor tileFlavor;*/
                                                
    static final int spacing = LocalSettings.TileSpacing;
    
    static final Border padding = BorderFactory.createEmptyBorder(spacing/2,spacing,spacing/2,spacing),
                        raisedbevel = BorderFactory.createRaisedBevelBorder(),
                        loweredbevel = BorderFactory.createLoweredBevelBorder(),
                        compoundbevel = BorderFactory.createCompoundBorder(raisedbevel, loweredbevel),
                        border = BorderFactory.createCompoundBorder(compoundbevel, padding);

    public Tile(Container layeredPane, final String text) {
        super(text);
        this.layeredPane = layeredPane; this.text = text;

        setFont(JapeFont.getFont(ProtocolConstants.TermFontNum));
        setBorder(border);
        setSize(getPreferredSize());

        /*if (tileFlavor==null) {
            try {
                tileFlavor = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType+
                                            "; class="+this.getClass().getName());
            } catch (ClassNotFoundException e) {
                Alert.abort("can't create tileFlavor");
            }
        }*/

        /*dragSource = new DragSource();
        dragSource.createDefaultDragGestureRecognizer(this, DnDConstants.ACTION_COPY, this);*/

        JapeMouseListener mil = new JapeMouseAdapter() {
            public void doubleclicked(MouseEvent e) {
                Reply.sendCOMMAND("tileact \""+text+"\"");
            }
            public void pressed(MouseEvent e) {
                Tile.this.pressed(e);
            }
            public void dragged(boolean wobbly, MouseEvent e) {
                if (wobbly) Tile.this.dragged(e);
            }
            public void released(MouseEvent e) {
                Tile.this.released(e);
            }
        };
        addMouseListener(mil);
        addMouseMotionListener(mil);
    }

    protected class TileImage extends Component {
        private BufferedImage image;
        private AlphaComposite comp;

        public TileImage() {
            super();
            setSize(Tile.this.getSize());
            int width = getWidth(), height = getHeight();
            image = (BufferedImage)Tile.this.createImage(width, height);
            Graphics imageGraphics = image.createGraphics();
            imageGraphics.setColor(Preferences.ProofBackgroundColour);
            imageGraphics.fillRect(0, 0, width, height);
            Tile.this.paint(imageGraphics);
            imageGraphics.dispose();
            comp = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, Preferences.Translucent);
        }

        public void paint(Graphics g) {
            if (g instanceof Graphics2D) {
                Graphics2D g2D = (Graphics2D)g;
                Composite oldcomp = g2D.getComposite();
                g2D.setComposite(comp);
                g.drawImage(image, 0,0, this);
                g2D.setComposite(oldcomp);
            }
            else
                Alert.abort("Tile.TileImage.paint can't enable transparent drawing");
        }
    }
    
    protected int startx, starty, lastx, lasty;
    private TileImage tileImage;

    protected void pressed(MouseEvent e) { // doesn't matter what keys are pressed
        if (drag_tracing)
            System.err.print("mouse pressed on tile "+text+" at "+e.getX()+","+e.getY()+
                             " insets="+getInsets());
        startx = lastx = e.getX(); starty = lasty = e.getY();
        if (tileImage==null)
            tileImage = new TileImage();
        layeredPane.add(tileImage, JLayeredPane.DRAG_LAYER);
        tileImage.setLocation(SwingUtilities.convertPoint(this, 0, 0, layeredPane));
        if (drag_tracing)
            System.err.println("; dragged tile at "+tileImage.getX()+","+tileImage.getY());
        tileImage.repaint();
    }

    protected void dragged(MouseEvent e) {
        if (drag_tracing)
            System.err.print("mouse dragged to "+e.getX()+","+e.getY());
        tileImage.repaint();
        tileImage.setLocation(tileImage.getX()+(e.getX()-lastx),
                                tileImage.getY()+(e.getY()-lasty));
        lastx = e.getX(); lasty = e.getY();
        if (drag_tracing)
            System.err.println("; dragged tile now at "+tileImage.getX()+","+tileImage.getY());
        tileImage.repaint();
    }

    protected void released(MouseEvent e) {
        if (drag_tracing)
            System.err.println("mouse released at "+e.getX()+","+e.getY()+
                               "; dragged tile at "+tileImage.getX()+","+tileImage.getY());
        layeredPane.remove(tileImage);
        layeredPane.repaint();
    }


   /* protected class TileTransferable implements Transferable {
        public Object getTransferData(DataFlavor flavor) {
            return Tile.this;
        }
        public DataFlavor[] getTransferDataFlavors() {
            return new DataFlavor[]{ tileFlavor };
        }
        public boolean isDataFlavorSupported(DataFlavor flavor) {
            return flavor==tileFlavor;
        }
    }

    public void dragGestureRecognized(DragGestureEvent event) {
        if (dragimage_tracing)
            System.err.println("isdragImageSupported()="+DragSource.isDragImageSupported());
        if (DragSource.isDragImageSupported()) {
            if (dragimage_tracing)
                System.err.println("dragging with drag Image support");
            dragSource.startDrag (event, DragSource.DefaultCopyDrop, new TileTransferable(), this);
        }
        else {
            Point origin = event.getDragOrigin();
            if (dragimage_tracing)
                System.err.println("dragging without drag Image support; dragOrigin="+origin);
            if (image==null) {
                int width = getWidth(), height = getHeight();
                image = (BufferedImage)createImage(width, height);
                Graphics imageGraphics = image.createGraphics();
                imageGraphics.setColor(Preferences.ProofBackgroundColour);
                imageGraphics.fillRect(0, 0, width, height);
                paint(imageGraphics);
                imageGraphics.dispose();
            }
            dragSource.startDrag(event, DragSource.DefaultCopyDrop, image,
                                 new Point(-origin.x, -origin.y),
                                 new TileTransferable(), this);
        }
    }

    public void dragEnter(DragSourceDragEvent event) { }

    public void dragExit(DragSourceEvent event) { }

    public void dragOver(DragSourceDragEvent event) { }

    public void dropActionChanged(DragSourceDragEvent event) { }

    public void dragDropEnd (DragSourceDropEvent event) {
    }*/
}
