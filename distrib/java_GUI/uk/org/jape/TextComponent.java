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

import java.awt.Font;

public class TextComponent {
    public final int offX, offY;
    public final byte fontnum;
    public final String printtext;
    public final int printlen;
    public final Font font;
    public final TextDimension dimension;
    public TextComponent(int x, int y, byte fontnum, String printtext) {
	this.offX = x; this.offY = y; this.fontnum = fontnum;
	this.printtext = printtext;
	this.printlen = printtext.length();
	this.font = JapeFont.getFont(fontnum);
	this.dimension = JapeFont.measure(printtext, fontnum);
    }
    
    public String toString() {
	return  "TextComponent["+offX+","+offY+" "+dimension+
	                       "fontnum="+fontnum+" "+
	                       printlen+":"+JapeUtils.enQuote(printtext)+"]";
    }
}
