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

import java.awt.BasicStroke;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;

// LineComponents can be dragged
public class LineComponent extends DragComponent implements DebugConstants {
    // but if you drag them, you must override this method
    protected void movePosition(int deltax, int deltay) {
        Alert.abort("dragging line without overriding movePosition");
    }
    
    protected int x0, y0, x1, y1, thickness;

    public LineComponent(int x0, int y0, int x1, int y1, int thickness) {
        super();
        resetLine(x0, y0, x1, y1, thickness);
        setForeground(Preferences.LineColour);
    }

    public void resetLine(int x0, int y0, int x1, int y1, int thickness) {
        setBounds(Math.min(x0,x1), Math.min(y0,y1),
                  Math.max(thickness, Math.abs(x1-x0)),
                  Math.max(thickness, Math.abs(y1-y0)));
        this.x0 = x0-getX(); this.y0 = y0-getY(); this.x1 = x1-getX(); this.y1 = y1-getY();
        this.thickness = thickness;
    }

    public void resetLine(int x0, int y0, int x1, int y1) {
        resetLine(x0, y0, x1, y1, thickness);
    }

    public void setlinethickness(int linethickness) {
        resetLine(x0+getX(), y0+getY(), x1+getX(), y1+getY(), linethickness);
    }

    public int thickness() { return thickness; }
    
    protected boolean stroked;
    protected int xfrom, yfrom, xto, yto;

    protected void prepaint(Graphics g) {
        g.setColor(getForeground());
        if (g instanceof Graphics2D) {
            BasicStroke stroke = new BasicStroke((float)thickness);
            int half = thickness/2;
            ((Graphics2D)g).setStroke(stroke);
            if (y0==y1) { // horizontal
                xfrom = x0; xto = x1;
                yfrom = yto = y0+half;
            }
            else
                if (x0==x1) { // vertical
                    xfrom = xto = x0+half;
                    yfrom = y0; yto = y1;
                }
            else {
                xfrom = x0; xto = x1;
                yfrom = y0; yto = y1;
            }
            stroked = true;
        }
        else {
            xfrom = x0<x1 ? x0 : x0-1; xto = x1<x0 ? x1 : x1-1;
            yfrom = y0<y1 ? y0 : y0-1; yto = y1<y0 ? y1 : y1-1;
            stroked = false;
        }
    }

    // default behaviour
    public void paint(Graphics g) {
        if (paint_tracing)
            Logger.log.println("painting line from "+xfrom+","+yfrom+" to "+xto+","+yto);
        prepaint(g); g.drawLine(xfrom, yfrom, xto, yto);
    }

    public String toString() {
        return "LineComponent["+super.toString()+
        "; x0="+x0+
        "; y0="+y0+
        "; x1="+x1+
        "; y1="+y1+
        "; thickness="+thickness+
        "]";
    }
}
