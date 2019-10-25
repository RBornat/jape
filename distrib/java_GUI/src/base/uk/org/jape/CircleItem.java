/* 
        Copyright © 2003-19 Richard Bornat & Bernard Sufrin
     
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

package uk.org.jape;

import java.awt.BasicStroke;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.Ellipse2D;

@SuppressWarnings("serial")
public class CircleItem extends OutlineItem implements DebugConstants {
    public final int xcentre, ycentre, innerRadius;
    protected Ellipse2D.Float outline;
    protected final int strokethickness;

    public CircleItem(JapeCanvas canvas, int xcentre, int ycentre, int innerRadius, int strokethickness) {
	super(canvas, xcentre-(innerRadius+strokethickness), ycentre-(innerRadius+strokethickness),
	      2*(innerRadius+strokethickness), 2*(innerRadius+strokethickness));
	this.xcentre = xcentre; this.ycentre = ycentre; this.innerRadius=innerRadius;
	this.strokethickness = strokethickness;
	outline = new Ellipse2D.Float(strokethickness, strokethickness,
				      2*innerRadius-strokethickness, 2*innerRadius-strokethickness);
    }

    public CircleItem(JapeCanvas canvas, int xcentre, int ycentre, int innerRadius) {
	this(canvas, xcentre, ycentre, innerRadius, canvas.linethickness);
    }

    public boolean contains(int x, int y) {
	return (x-innerRadius)*(x-innerRadius)+(y-innerRadius)*(y-innerRadius)
			<=innerRadius*innerRadius;
    }

    public void paint(Graphics g) {
	if (DebugVars.paint_tracing)
	    Logger.log.println("painting circle at "+getX()+","+getY());
	g.setColor(getForeground());
	if (g instanceof Graphics2D) {
	    if (antialias_tracing) {
		Logger.log.print("circle hints "+((Graphics2D)g).getRenderingHints());
		if (Jape.onMacOSX)
		    Logger.log.println(" hwaccel "+System.getProperty("com.apple.hwaccel"));
		else
		    Logger.log.println();
	    }
	    BasicStroke stroke = new BasicStroke((float)strokethickness);
	    ((Graphics2D)g).setStroke(stroke);
	    ((Graphics2D)g).draw(outline);
	}
	else
	    g.drawOval(canvas.linethickness, canvas.linethickness, 2*innerRadius, 2*innerRadius);
    }
}