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

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;

public class WorldCanvas extends JapeCanvas {

    public WorldCanvas(Container viewport, boolean scrolled) {
        super(viewport, scrolled);
    }

    public String getSelections(String sep) {
        Alert.abort("WorldCanvas.getSelections");
        return ""; // shut up compiler
    }
    
    public String getTextSelections(String sep) {
        Alert.abort("WorldCanvas.getTextSelections");
        return ""; // shut up compiler
    }

    public int worldRadius() {
        return 5*linethickness;
    }
    
    public void worldsStart() {
        removeAll();
    }

    public WorldItem findWorld(int x, int y, boolean musthave) throws ProtocolError {
        int nc = child.getComponentCount(); // oh dear ...
        for (int i=0; i<nc; i++) {
            Component c = child.getComponent(i); // oh dear ...
            if (c instanceof WorldItem &&
                ((WorldItem)c).idX==x && ((WorldItem)c).idY==y)
                return (WorldItem)c;
        }
        if (musthave)
            throw new ProtocolError("no world at "+x+","+y);
        else
            return null;
    }

    public void addWorld(int x, int y) throws ProtocolError /* doesn't! */ {
        if (findWorld(x,y,false)==null)
            add(new WorldItem(this,x,y));
    }

    public void addWorldLabel(int x, int y, String label) throws ProtocolError {
        findWorld(x,y,true);
        System.err.println("no world labels yet");
    }

    public void addChildWorld(int x, int y, int xc, int yc) {
        System.err.println("no child worlds yet");
    }

    public void selectWorld(int x, int y, boolean selected) throws ProtocolError {
        findWorld(x,y,true).select(selected);
    }
}