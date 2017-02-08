/* 
        Copyright © 2003-17 Richard Bornat & Bernard Sufrin
     
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

import javax.swing.event.MouseInputListener;

@SuppressWarnings("serial")
public class LineItem extends LineComponent {

    protected final JapeCanvas canvas;

    public LineItem(JapeCanvas canvas, int x0, int y0, int x1, int y1) {
	super(x0, y0, x1, y1, canvas.linethickness);
	this.canvas = canvas;
    }

    public void addMouseInputListener(MouseInputListener a) {
	addMouseListener(a); addMouseMotionListener(a); 
    }
}
