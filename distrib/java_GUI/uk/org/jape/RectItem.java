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
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;

public class RectItem extends Component implements DebugConstants {

    JapeCanvas canvas;
    
    public RectItem(JapeCanvas canvas, int x, int y, int w, int h) {
        super();
        this.canvas = canvas;
        setBounds(x, y, w, h);
        setForeground(Preferences.LineColour);
    }

    protected int left, top, right, bottom;
    protected boolean stroked;
    
    protected void prepaint(Graphics g) {
        g.setColor(getForeground());
        if (g instanceof Graphics2D) {
            int half = canvas.linethickness/2;
            int otherhalf = canvas.linethickness-canvas.linethickness/2;
            BasicStroke stroke = new BasicStroke((float)canvas.linethickness);
            ((Graphics2D)g).setStroke(stroke);
            left = top = half; // seems to work
            right = getWidth()-otherhalf;
            bottom = getHeight()-otherhalf;
            stroked = true;
        }
        else {
            left = top = 0;
            right = getWidth()-1; bottom = getHeight()-1;
            stroked = false;
        }
    }

    protected void paintHorizEdge(Graphics g, int y) {
        g.drawLine(left,y, right,y);
    }

    protected void paintVertEdge(Graphics g, int x) {
        g.drawLine(x,top, x,bottom);
    }

    protected void paintSides(Graphics g) {
        paintVertEdge(g,left); paintVertEdge(g,right);
    }

    protected void paintTopAndBottom(Graphics g) {
        paintHorizEdge(g,top); paintHorizEdge(g,bottom);
    }

    // default behaviour -- drawLine seems to work more nicely than drawRect ...
    public void paint(Graphics g) {
        if (paint_tracing)
            System.err.println("painting rectangle at "+getX()+","+getY());
        prepaint(g); paintTopAndBottom(g); paintSides(g);
    }
}
