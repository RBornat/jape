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

public abstract class EmphasisableItem extends TextSelectableItem {

    protected EmphasisLine emphasisLine;

    protected final DisproofCanvas canvas;

    public EmphasisableItem(DisproofCanvas canvas, int x, int y, byte fontnum,
                            String annottext, String printtext) {
        super(canvas,x,y,fontnum,annottext,printtext);
        this.canvas = canvas;
        emphasisLine = new EmphasisLine(false, getX(), getY()+getHeight()+canvas.getSurroundGap(), getWidth());
        canvas.add(emphasisLine);
    }

    protected class EmphasisLine extends LineItem {

        private boolean emphasised;

        EmphasisLine(boolean emphasised, int x1, int y1, int width) {
            super(EmphasisableItem.this.canvas, x1, y1, x1+width, y1);
            setForeground(Preferences.ForcedColour);
            this.emphasised = emphasised;
        }

        public void paint(Graphics g) {
            if (emphasised)
                super.paint(g);
        }

        public void emphasise(boolean emphasised) {
            this.emphasised = emphasised; repaint();
        }
    }

    public void emphasise(boolean emphasised) {
        emphasisLine.emphasise(emphasised);
    }
    
}
