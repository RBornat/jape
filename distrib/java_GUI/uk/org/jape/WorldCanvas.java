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

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;

import java.awt.image.BufferedImage;

public class WorldCanvas extends JapeCanvas implements DebugConstants {

    protected RenderingHints renderingHints;
    
    public WorldCanvas(Container viewport, boolean scrolled) {
        super(viewport, scrolled);
        renderingHints = new RenderingHints(RenderingHints.KEY_ANTIALIASING,
                                            RenderingHints.VALUE_ANTIALIAS_ON);
        renderingHints.put(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
    }

    // It is worth demanding good antialiasing when drawing blobs, rings, diagonal lines.
    // The text can come along for the ride.

    // Since antialiasing may be expensive, I construct an image in a buffer and blit it
    // into the canvas when needed.  It's rebuilt only when the component population changes
    // or when we do drag-under highlighting.

    BufferedImage image;
    Graphics imageGraphics;
    private boolean imageRepaint = true;

    public void imageRepaint() { imageRepaint = true; } // ho ho!
    
    public Component add(Component c) {
        if (antialias_tracing)
            System.err.println("WorldCanvas adding "+c);
        imageRepaint = true;
        return super.add(c);
    }

    public Component add(Component c, int index) {
        if (antialias_tracing)
            System.err.println("WorldCanvas adding "+c+" at "+index);
        imageRepaint = true;
        return super.add(c, index);
    }

    public void remove(Component c) {
        if (antialias_tracing)
            System.err.println("WorldCanvas removing "+c);
        imageRepaint = true;
        super.remove(c);
    }

    public void removeAll() {
        if (antialias_tracing)
            System.err.println("WorldCanvas removeAll");
        imageRepaint = true;
        super.removeAll();
    }

    private static Rectangle clip = new Rectangle();

    public void paint(Graphics g) {
        if (paint_tracing)
            System.err.println("painting WorldCanvas");
        if (imageRepaint) {
            int width = getWidth(), height = getHeight();
            if (antialias_tracing)
                System.err.println("painting image "+g.getClipBounds());
            // if the image changes size, we need a new buffer
            if (image==null || image.getWidth()!=width || image.getHeight()!=height) {
                if (antialias_tracing)
                    System.err.println("new image buffer");
                image = (BufferedImage)createImage(width, height);
                imageGraphics = image.createGraphics();
                if (imageGraphics instanceof Graphics2D) {
                    if (antialias_tracing)
                        System.err.println("pre enhancedPaint hints "+
                                           ((Graphics2D)imageGraphics).getRenderingHints());
                    ((Graphics2D)imageGraphics).addRenderingHints(renderingHints);
                    if (antialias_tracing) {
                        System.err.println("enhancedPaint hints "+
                                           ((Graphics2D)imageGraphics).getRenderingHints());
                    }
                }
                imageGraphics.setColor(Preferences.ProofBackgroundColour);
                imageGraphics.fillRect(0, 0, width, height);
            }
            g.getClipBounds(clip);
            imageGraphics.setClip(clip.x, clip.y, clip.width, clip.height);
            super.paint(imageGraphics);
            imageRepaint = false;
        }
        if (antialias_tracing)
            System.err.println("blitting image "+g.getClipBounds());
        g.drawImage(image, 0, 0, this);
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

    public void addWorld(int x, int y) throws ProtocolError /* doesn't! */ {
        if (findWorld(x,y,false)==null)
            add(new WorldItem(this,x,y));
    }

    public void addWorldLabel(int x, int y, String label) throws ProtocolError {
        findWorld(x,y,true).addlabel(label);
    }

    public void addChildWorld(int x, int y, int xc, int yc) {
        System.err.println("no child worlds yet");
    }

    public void selectWorld(int x, int y, boolean selected) throws ProtocolError {
        findWorld(x,y,true).select(selected);
    }

    public TextItem addLabelItem(int x, int y, String label) {
        return (TextItem)add(new TextItem(this, x, y, ProtocolConstants.ProvisoFontNum, label, label));
    }
}