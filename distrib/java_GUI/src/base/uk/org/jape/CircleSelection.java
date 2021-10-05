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

import java.awt.Graphics;

@SuppressWarnings("serial")
public class CircleSelection extends CircleItem
			  implements SelectionIndicator {

    public CircleSelection(DisplayItem item) {
	super(item.canvas, item.getX()+item.getWidth()/2, item.getY()+item.getHeight()/2,
	      Math.max(item.getWidth(),item.getHeight())/2+2*item.canvas.linethickness);
    }

    private boolean selected;

    public void indicate(DisplayItem item) {
	selected = item.getSelected();
    }

    public void paint(Graphics g) {
	if (selected) {
	    setForeground(JapePrefs.SelectionColour);
	    super.paint(g);
	}
    }
}
