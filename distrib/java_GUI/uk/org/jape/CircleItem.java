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
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

public class CircleItem extends Component {

    JapeCanvas canvas;
    int radius;
    protected RenderingHints renderingHints;
    
    public CircleItem(JapeCanvas canvas, int x, int y, int radius) {
        super();
        this.canvas = canvas; this.radius=radius;
        int width = 2*(radius+canvas.linethickness);
        setBounds(x-radius-canvas.linethickness, y-radius-canvas.linethickness, width, width);
        setForeground(Preferences.LineColour);
        renderingHints = new RenderingHints(RenderingHints.KEY_ANTIALIASING,
                                            RenderingHints.VALUE_ANTIALIAS_ON);
    }

    public void paint(Graphics g) {
        g.setColor(getForeground());
        if (g instanceof Graphics2D) {
            BasicStroke stroke = new BasicStroke((float)canvas.linethickness);
            ((Graphics2D)g).setStroke(stroke);
            ((Graphics2D)g).addRenderingHints(renderingHints);
        }
        g.drawOval(canvas.linethickness, canvas.linethickness, 2*radius, 2*radius);
    }
}