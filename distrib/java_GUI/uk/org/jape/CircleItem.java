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
import java.awt.Component;
import java.awt.geom.Ellipse2D;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

public class CircleItem extends Component implements DebugConstants {

    JapeCanvas canvas;
    int radius;
    protected Ellipse2D.Float outline;
    protected final int strokethickness;
    
    public CircleItem(JapeCanvas canvas, int x, int y, int radius) {
        super();
        this.canvas = canvas; this.radius=radius; strokethickness = canvas.linethickness;
        int width = 2*(radius+strokethickness);
        setBounds(x-radius-strokethickness, y-radius-strokethickness, width, width);
        setForeground(Preferences.LineColour);
        outline = new Ellipse2D.Float(strokethickness, strokethickness, 2*radius, 2*radius);
    }

    public void paint(Graphics g) {
        if (paint_tracing)
            System.err.println("painting circle at "+getX()+","+getY());
        g.setColor(getForeground());
        if (g instanceof Graphics2D) {
            if (antialias_tracing) {
                System.err.print("circle hints "+((Graphics2D)g).getRenderingHints());
                if (japeserver.onMacOS)
                    System.err.println(" hwaccel "+System.getProperty("com.apple.hwaccel"));
                else
                    System.err.println();
            }
            BasicStroke stroke = new BasicStroke((float)strokethickness);
            ((Graphics2D)g).setStroke(stroke);
            ((Graphics2D)g).draw(outline);
        }
        else
            g.drawOval(canvas.linethickness, canvas.linethickness, 2*radius, 2*radius);
    }
}