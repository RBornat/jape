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

import java.awt.Graphics;

public class WorldItem extends DisplayItem {

    protected WorldCanvas canvas;
    protected SelectionRing selectionRing;
    
    public WorldItem(WorldCanvas canvas, int x, int y) {
        super(x, y);
        this.canvas = canvas;
        int x0 = x, y0 = -y-2*canvas.getWorldRadius();
        setBounds(x0-canvas.getWorldRadius(), y0-canvas.getWorldRadius(),
                  2*canvas.getWorldRadius(), 2*canvas.getWorldRadius());
        selectionRing = new SelectionRing(x0, y0, canvas.getWorldRadius()+2*canvas.linethickness);
        canvas.add(selectionRing);
    }

    public void select(boolean selected) {
        selectionRing.select(selected);
    }

    public void paint(Graphics g) {
        g.setColor(Preferences.WorldColour); g.fillOval(0, 0, getWidth(), getHeight());
    }
    
    protected class SelectionRing extends CircleItem {
        private boolean selected;

        SelectionRing(int x, int y, int radius) {
            super(WorldItem.this.canvas, x, y, radius);
            setForeground(Preferences.SelectionColour);
        }

        public void paint(Graphics g) {
            if (selected) super.paint(g);
        }

        public void select(boolean selected) {
            this.selected = selected; repaint();
        }
    }
}