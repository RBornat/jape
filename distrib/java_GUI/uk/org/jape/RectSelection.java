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

import java.awt.Graphics;
import java.awt.Rectangle;

public class RectSelection extends RectItem
                        implements SelectionIndicator {

    public RectSelection(DisplayItem item) {
        this(item.canvas, item.getBounds(), item.canvas.getSelectionGap());
    }

    private RectSelection(JapeCanvas canvas, Rectangle bounds, int gap) {
        super(canvas, bounds.x-gap, bounds.y-gap, bounds.width+2*gap, bounds.height+2*gap);
    }

    private boolean selected;
                                     
    public void indicate(DisplayItem item) {
        selected = item.getSelected();
        repaint();
    }

    protected boolean getSelected() {
        return selected;
    }
    
    public void paint(Graphics g) {
        if (selected) {
            setForeground(Preferences.SelectionColour);
            super.paint(g);
        }
    }
 }
