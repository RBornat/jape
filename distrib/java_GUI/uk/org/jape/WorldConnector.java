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

import java.awt.Color;

import javax.swing.JFrame;

public class WorldConnector extends LineItem implements SelectionConstants, WorldTarget {
    public final WorldItem from, to;
    public WorldCanvas canvas;
    public WorldConnector(WorldCanvas canvas, JFrame window, WorldItem from, WorldItem to) {
        super(canvas, from.idX, -from.idY, to.idX, -to.idY);
        this.canvas = canvas;
        this.from = from; this.to = to;
        from.registerFrom(this); to.registerTo(this);
    }

    /* ****************************** world as drag target ****************************** */

    private static final int wobble = 3;
    
    public boolean contains(int x, int y) {
        if (-wobble<=x && x<getWidth()+wobble && -wobble<=y && y<getHeight()+wobble) {
            if (x0==x1)
                return Math.abs(x-x0)<=wobble; // vertical
            else
            if (y0==y1)
                return  Math.abs(y-y0)<=wobble; // horizontal
            else {
                float a = y1-y0, b = x0-x1, c = a*x0+b*y0;
                float dx = x-(c-b*y)/a, dy = y-(c-a*x)/b;
                return  Math.abs(dx)<=wobble ||  Math.abs(dy)<=wobble ||
                        dx*dx*dy*dy/(dx*dx+dy*dy)<=wobble*wobble;
            }
        }
        else
            return false;
    }

    private boolean draghighlight;
    private Color previousColour;
    
    public boolean dragEnter(Object o) {
        if (o instanceof WorldItem) {
            previousColour = getForeground();
            setForeground(Preferences.SelectionColour);
            canvas.imageRepaint(); repaint();
            draghighlight = true;
            return true;
        }
        else
            return false;
    }

    public void dragExit() {
       setForeground(previousColour);
        canvas.imageRepaint(); repaint();
        draghighlight = false;
    }

    /* ****************************** world as drop target ****************************** */

    public void drop(byte dragKind, WorldItem w, int x, int y) {
        if (draghighlight) {
            Reply.sendCOMMAND((dragKind==MoveWorldDrag ? "moveworldtolink" : "addworldtolink")+
                              " "+w.idX+" "+w.idY+
                              " "+(x+getX())+" "+(-(y+getY()))+
                              " "+from.idX+" "+from.idY+" "+to.idX+" "+to.idY);
            dragExit();
        }
        else
            Alert.abort("world drop on non-accepting line");
    }
}
