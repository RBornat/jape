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
    protected final byte          fontnum;
    protected final Font          font;
    protected final TextDimension dimension;
    protected final String        text;

    public TextItem(JapeCanvas canvas, int x, int y, byte fontnum, String text) { 
        super(canvas, x, y);
        this.fontnum = fontnum;
        this.font = JapeFont.getFont(fontnum);
        this.text = text;
        this.dimension = JapeFont.measure(text, fontnum);
        if (fontDebug)
            Logger.log.println(this);
        setBounds((int)x, y-dimension.ascent, dimension.width, dimension.ascent+dimension.descent);
        setForeground(Preferences.TextColour);
    }

    public void paint(Graphics g) {
        if (paint_tracing || fontDebug)
            Logger.log.println("painting "+this);
        g.setFont(font);
        g.setColor(isEnabled() ? getForeground() : Preferences.GreyTextColour);
        g.drawString(text, 0, dimension.ascent);
    }

    public void blacken() {
        setEnabled(true);
    }

    public void greyen() {
        setEnabled(false);
    }
    
    // this isn't efficient, but that doesn't matter, I think
    public String toString() {
        return "TextItem["+
               "text="+JapeUtils.enQuote(text)+
               ", fontnum="+fontnum+", font="+font+
               ", dimension="+dimension+
               ", "+super.toString()+"]";
    }
}



