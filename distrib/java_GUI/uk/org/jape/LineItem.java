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

public class LineItem extends Component implements DebugConstants {

    protected final JapeCanvas canvas;
    private int x1, y1, x2, y2;

    public LineItem(JapeCanvas canvas, int x1, int y1, int x2, int y2) {
        super();
        this.canvas = canvas;
        setBounds(Math.min(x1,x2), Math.min(y1,y2),
                  Math.max(canvas.linethickness, Math.abs(x2-x1)),
                  Math.max(canvas.linethickness, Math.abs(y2-y1)));
        this.x1 = x1-getX(); this.y1 = y1-getY(); this.x2 = x2-getX(); this.y2 = y2-getY();
        setForeground(Preferences.LineColour);
    }

    protected boolean stroked;
    protected int xfrom, yfrom, xto, yto;

    protected void prepaint(Graphics g) {
        g.setColor(getForeground());
        if (g instanceof Graphics2D) {
            BasicStroke stroke = new BasicStroke((float)canvas.linethickness);
            int half = canvas.linethickness/2;
            ((Graphics2D)g).setStroke(stroke);
            if (y1==y2) { // horizontal
                xfrom = x1; xto = x2;
                yfrom = yto = y1+half;
            }
            else
            if (x1==x2) { // vertical
                xfrom = xto = x1+half;
                yfrom = y1; yto = y2;
            }
            else {
                xfrom = x1; xto = x2;
                yfrom = y1; yto = y2;
            }
            stroked = true;
        }
        else {
            xfrom = x1<x2 ? x1 : x1-1; xto = x2<x1 ? x2 : x2-1;
            yfrom = y1<y2 ? y1 : y1-1; yto = y2<y1 ? y2 : y2-1;
            stroked = false;
        }
    }

    // default behaviour
    public void paint(Graphics g) {
        if (paint_tracing)
            System.err.println("painting line from "+xfrom+","+yfrom+" to "+xto+","+yto);
        prepaint(g); g.drawLine(xfrom, yfrom, xto, yto);
    }
}
