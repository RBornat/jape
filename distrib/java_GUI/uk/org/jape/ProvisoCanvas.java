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

public class ProvisoCanvas extends JapeCanvas {

    private int provisocount, givencount;

    public ProvisoCanvas(Container viewport, boolean scrolled) {
        super(viewport, scrolled);
        provisocount = givencount = 0;
    }

    public String getSelections(String sep) {
        Alert.abort("ProvisoCanvas.getSelections");
        return ""; // shut up compiler
    }
    
    // not efficient, not in time order
    // always ends with a blank line
    public String getTextSelections(String sep) {
        String s = null;
        int nc = child.getComponentCount(); // oh dear ...
        for (int i=0; i<nc; i++) {
            Component c = child.getComponent(i); // oh dear ...
            if (c instanceof TextSelectableItem) {
                TextSelectableItem sti = (TextSelectableItem)c;
                String s1 = sti.getTextSelections();
                if (s1!=null) {
                    if (s==null)
                        s=s1;
                    else
                        s=s+sep+s1;
                }
            }
        }
        return s;
    }
    public Component add(Component c) {
        Alert.abort("ProvisoCanvas.add("+c+")");
        return c; // shut up compiler
    }

    public Component add(Component c, int index) {
        Alert.abort("ProvisoCanvas.add("+c+", "+index+")");
        return c; // shut up compiler
    }

    public void remove(Component c) {
        Alert.abort("ProvisoCanvas.remove("+c+")");
    }

    public void removeAll() {
        Alert.abort("ProvisoCanvas.removeAll()");
    }

    public void clear() {
        super.removeAll();
        setOrigin(0,0); // maybe ...
    }

    public void addProvisoLine(String annottext) {
        
    }
}
