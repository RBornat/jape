/* 
    $Id$

    Copyright © 2003 Richard Bornat & Bernard Sufrin
     
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

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;

import java.awt.image.BufferedImage;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;

public class WorldCanvas extends JapeCanvas implements DebugConstants, WorldTarget {

    protected RenderingHints renderingHints;
    protected JFrame window;
    protected WasteBin wasteBin;
    
    public WorldCanvas(Container viewport, boolean scrolled,
                       JFrame window, WasteBin wasteBin, int linethickness) {
        super(viewport, scrolled);
        this.window = window;
        this.wasteBin = wasteBin;
        setlinethickness(linethickness); 
        
        renderingHints = new RenderingHints(RenderingHints.KEY_ANTIALIASING,
                                            RenderingHints.VALUE_ANTIALIAS_ON);
        renderingHints.put(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        
        Point origin = getOrigin();
        setOrigin(origin.x, origin.y+2*worldRadius());
        /*setDropTarget(new DropTarget(this, this));*/
    }

    protected void claimFocus() {
        getProofWindow().claimDisproofFocus();
    }

    // It is worth demanding good antialiasing when drawing blobs, rings, diagonal lines.
    // The text can come along for the ride.

    // Since antialiasing may be expensive, I construct an image in a buffer and blit it
    // into the canvas when needed.  It's rebuilt only when the component population changes
    // or when we do drag-under highlighting.

    BufferedImage image;
    Graphics imageGraphics;
    private boolean imageRepaint = true;

    public void imageRepaint() {
        if (worldpaint_tracing)
            Logger.log.println("WorldCanvas.imageRepaint");
        imageRepaint = true;
    } // ho ho!
    
    public Component add(Component c) {
        if (worldpaint_tracing)
            Logger.log.println("WorldCanvas adding "+c);
        imageRepaint = true;
        return super.add(c);
    }

    public Component add(Component c, int index) {
        if (worldpaint_tracing)
            Logger.log.println("WorldCanvas adding "+c+" at "+index);
        imageRepaint = true;
        return super.add(c, index);
    }

    public void remove(Component c) {
        if (worldpaint_tracing)
            Logger.log.println("WorldCanvas removing "+c);
        imageRepaint = true;
        super.remove(c);
    }

    public void removeAll() {
        if (worldpaint_tracing)
            Logger.log.println("WorldCanvas removeAll");
        imageRepaint = true;
        super.removeAll();
    }

    public void forcerepaint() {
        if (worldpaint_tracing)
            Logger.log.println("WorldCanvas forcerepaint");
        imageRepaint = true;
        repaint();
    }

    private static Rectangle clip = new Rectangle();

    public void paint(Graphics g) {
        if (worldpaint_tracing) 
            Logger.log.println("painting WorldCanvas "+getWidth()+","+getHeight());
        if (imageRepaint) {
            imageRepaint = false;
            int width = getWidth(), height = getHeight();
            // if the image changes size, we need a new buffer
            if (image==null || image.getWidth()!=width || image.getHeight()!=height) {
                if (worldpaint_tracing)
                    Logger.log.println("new image buffer");
                image = (BufferedImage)createImage(width, height);
                imageGraphics = image.createGraphics();
                if (imageGraphics instanceof Graphics2D) {
                    if (antialias_tracing)
                        Logger.log.println("pre enhancedPaint hints "+
                                           ((Graphics2D)imageGraphics).getRenderingHints());
                    ((Graphics2D)imageGraphics).addRenderingHints(renderingHints);
                    if (antialias_tracing) {
                        Logger.log.println("enhancedPaint hints "+
                                           ((Graphics2D)imageGraphics).getRenderingHints());
                    }
                }
                imageGraphics.setClip(0, 0, width, height);
                if (worldpaint_tracing)
                    Logger.log.println("painting entire image");
            }
            else {
                g.getClipBounds(clip);
                imageGraphics.setClip(clip.x, clip.y, clip.width, clip.height);
                if (worldpaint_tracing)
                    Logger.log.println("painting image "+g.getClipBounds());
            }
            if (worldpaint_tracing)
                JapeUtils.showContainer(this, null);
            imageGraphics.setColor(Preferences.ProofBackgroundColour);
            imageGraphics.fillRect(0, 0, width, height);
            super.paint(imageGraphics);
        }
        if (worldpaint_tracing)
            Logger.log.println("blitting image "+g.getClipBounds());
        g.drawImage(image, 0, 0, this);
    }

    // draw directly to print graphics
    public void print(Graphics g) {
        super.paint(g);
    }
    
    public String getSelections(String sep) {
        Alert.abort("WorldCanvas.getSelections");
        return ""; // shut up compiler
    }
    
    public String getTextSelections(String sep) {
        Alert.abort("WorldCanvas.getTextSelections");
        return ""; // shut up compiler
    }

    public int worldRadius() {
        return 5*linethickness;
    }
    
    public void worldsStart() {
        removeAll();
    }

    public WorldItem findWorld(int x, int y, boolean musthave) throws ProtocolError {
        int nc = child.getComponentCount(); // oh dear ...
        for (int i=0; i<nc; i++) {
            Component c = child.getComponent(i); // oh dear ...
            if (c instanceof WorldItem &&
                ((WorldItem)c).idX==x && ((WorldItem)c).idY==y)
                return (WorldItem)c;
        }
        if (musthave)
            throw new ProtocolError("no world at "+x+","+y);
        else
            return null;
    }

    private WorldItem ensureWorld(int x, int y) {
        try {
            WorldItem w;
            if ((w=findWorld(x,y,false))==null)
                return (WorldItem)add(new WorldItem(this, window, x, y));
            else
                return w;
        } catch (ProtocolError e) {
            Alert.abort("WorldCanvas.ensureWorld error "+e.getMessage());
            return null; // shut up compiler
        }
    }
    
    public void addWorld(int x, int y) {
        ensureWorld(x, y);
    }

    public int worldCount() {
        int nc = child.getComponentCount(), nw=0;
        for (int i=0; i<nc; i++)
            if (child.getComponent(i) instanceof WorldItem)
                nw++;
        return nw;
    }
    
    public void addWorldLabel(int x, int y, String label) throws ProtocolError {
        findWorld(x,y,true).addlabel(label);
    }

    public void addChildWorld(int x, int y, int xc, int yc) {
        WorldItem wfrom = ensureWorld(x, y), wto = ensureWorld(xc, yc);
        add(new WorldConnector(this, window, wfrom, wto));
    }

    public void selectWorld(int x, int y, boolean selected) throws ProtocolError {
        findWorld(x,y,true).select(selected);
    }

    /* ****************************** canvas as drag target ****************************** */

    public boolean dragEnter(byte dragKind, WorldItem w) { return true; }
    
    public void dragExit(byte dragKind, WorldItem w) { }

    /* ****************************** canvas as drop target ****************************** */
    
    public void drop(byte dragKind, WorldItem w, int x, int y) {
        Reply.sendCOMMAND((dragKind==MoveWorldDrag ? "moveworld" : "addworld")+
                            " "+w.idX+" "+w.idY+" "+(x-child.getX())+" "+(-(y-child.getY())));
    }

 /* // Called when a drag operation has encountered the DropTarget.
    public void dragEnter(DropTargetDragEvent event) {
        if (dragimage_tracing)
            Logger.log.println("Canvas dragEnter "+event.getLocation());
        if (event.isDataFlavorSupported(WorldItem.worldFlavor) &&
            // event.isLocalTransfer() && -- why can't we do this?
            event.getDropAction()==DnDConstants.ACTION_MOVE) {
            event.acceptDrag(DnDConstants.ACTION_MOVE);
        }
        else 
        if (dragimage_tracing)
            Logger.log.println("not completely recognised: worldFlavorsupported="+
                               event.isDataFlavorSupported(WorldItem.worldFlavor)+
                               "; event.getDropAction()="+event.getDropAction()+
                               "; DnDConstants.ACTION_MOVE="+DnDConstants.ACTION_MOVE);
        Component c = event.getDropTargetContext().getComponent();
        if (dragimage_tracing) {
            if (c instanceof WorldItem) {
                Logger.log.println("in layeredPane coordinates we are at "+
                                   SwingUtilities.convertPoint(c, event.getLocation(), layeredPane));
            }
            else
                Logger.log.println("drag Component is "+c);
        }
    }

    // The drag operation has departed the DropTarget without dropping.
    public void dragExit(DropTargetEvent dte) {
    }

    // Called when a drag operation is ongoing on the DropTarget.
    public void dragOver(DropTargetDragEvent event) {
    }

    // The drag operation has terminated with a drop on this DropTarget.
    public void drop(DropTargetDropEvent event) {
        // I don't yet know how to avoid inter-window dragging ... perhaps this isn't a good idea!
        if (event.isDataFlavorSupported(WorldItem.worldFlavor)) {
            event.acceptDrop(DnDConstants.ACTION_MOVE);
            try {
                WorldItem w = ((WorldItem)event.getTransferable().
                                      getTransferData(WorldItem.worldFlavor));
                if (dragimage_tracing)
                    Logger.log.println("in layeredPane coordinates drop at "+
                                       SwingUtilities.convertPoint(w, event.getLocation(), layeredPane));
                w.draggedTo(event.getLocation());
                event.dropComplete(true);
            } catch (Exception e) {
                Alert.abort("drop onto world not worldFlavor");
            }
        }
        else
            event.dropComplete(false);
    }

    // Called if the user has modified the current drop gesture
    public void dropActionChanged(DropTargetDragEvent event) {
    }*/
}