/* 
    $Id$

    Copyright © 2003 Richard Bornat & Bernard Sufrin
     
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
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;

import java.util.Vector;

import java.awt.image.BufferedImage;

public class DragImage extends DragComponent implements DebugConstants {
    private AlphaComposite comp;
    
    public DragImage(float opacity) {
        super();
        setBackground(Preferences.ProofBackgroundColour);
        comp = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opacity);
    }

    private Rectangle imagebounds;
    private Vector includev = new Vector();
    
    public void include(Component c) {
        if (includev.indexOf(c)==-1) {
            includev.add(c);
        }
        if (imagebounds==null)
            imagebounds = c.getBounds();
        else
            imagebounds.add(c.getBounds());
    }

    private BufferedImage image;
    
    public void fixImage() {
        if (includev.size()==0)
            Alert.abort("DragImage.fixImage no items");

        setSize(imagebounds.width, imagebounds.height);
        image = (BufferedImage)((Component)includev.get(0)).createImage(imagebounds.width, imagebounds.height);
        Graphics imageGraphics = image.createGraphics();
        imageGraphics.setClip(0, 0, imagebounds.width, imagebounds.height);
        imageGraphics.setColor(getBackground());
        imageGraphics.fillRect(0, 0, imagebounds.width, imagebounds.height);
        for (int i=0; i<includev.size(); i++) {
            Component c = (Component)includev.get(i);
            int deltax = c.getX()-imagebounds.x, deltay = c.getY()-imagebounds.y;
            imageGraphics.translate(deltax, deltay);
            if (drag_tracing)
                System.err.println("fixImage painting "+c+" @ ("+deltax+","+deltay+")");
            c.paint(imageGraphics);
            imageGraphics.translate(-deltax, -deltay);
        }
        imageGraphics.dispose();
    }

    public Point getImageLocation() {
        if (imagebounds==null)
            Alert.abort("DragImage.getImageLocation no image");

        return new Point(imagebounds.x, imagebounds.y);
    }
    
    public void paint(Graphics g) {
        if (image==null)
            fixImage();
        if (g instanceof Graphics2D) {
            Graphics2D g2D = (Graphics2D)g;
            Composite oldcomp = g2D.getComposite();
            g2D.setComposite(comp);
            g.drawImage(image, 0,0, this);
            g2D.setComposite(oldcomp);
        }
        else
            Alert.abort("DragImage.paint can't enable transparent drawing");
    }

    protected void movePosition(int deltax, int deltay) {
        setLocation(getX()+deltax, getY()+deltay);
    }
}
