/* 
    $Id$

    Copyright © 2003-4 Richard Bornat & Bernard Sufrin
     
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;

class TextItem extends DisplayItem implements DebugConstants {
    protected final TextComponent[] components;
    protected final int ascent, descent;

    public TextItem(JapeCanvas canvas, int x, int y, byte fontnum, String text) {
	this(canvas, x, y, new TextComponent[]{new TextComponent(0, 0, fontnum, text)});
    }
    
    public TextItem(JapeCanvas canvas, int x, int y, TextComponent[] components){ 
	super(canvas, x, y);
	this.components = components;
	int left=x, top=y, right=x, bottom=y;
	for (int i=0; i<components.length; i++) {
	    TextComponent c = components[i];
	    TextDimension d = c.dimension;
	    int cLeft = x+c.offX, 
		cTop = y+c.offY-d.ascent,
		cRight = cLeft+d.width,
		cBottom = cTop+d.ascent+d.descent;
	    left = Math.min(left, cLeft);
	    top = Math.min(top, cTop);
	    right = Math.max(right, cRight);
	    bottom = Math.max(bottom, cBottom);
	}
	ascent = y-top; descent = bottom-y;
	if (fontDebug)
	    Logger.log.println(this);
	setBounds(left, top, right-left, bottom-top);
	setForeground(Preferences.TextColour);
    }

    public void paint(Graphics g) {
	if (paint_tracing || fontDebug)
	    Logger.log.println("painting "+this);
	for (int i=0; i<components.length; i++) {
	    TextComponent c = components[i];
	    g.setFont(c.font);
	    g.setColor(isEnabled() ? getForeground() : Preferences.GreyTextColour);
	    g.drawString(c.printtext, c.offX, ascent+c.offY);
	}
    }

    public void blacken() {
	setEnabled(true);
    }

    public void greyen() {
	setEnabled(false);
    }
    
    public String toString() {
	String s = "TextItem["+"components=[";
	for (int i=0; i<components.length; i++) {
	    s = s+components[i];
	    if (i+1<components.length)
		s = s+"; ";
	}
	return s+", ascent="+ascent+", descent="+descent+"], "+super.toString()+"]";
    }
}



