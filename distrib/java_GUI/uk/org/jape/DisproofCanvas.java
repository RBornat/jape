/* 
    $Id$

    Copyright © 2003 Richard Bornat & Bernard Sufrin
     
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

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;

public class DisproofCanvas extends JapeCanvas {

    public DisproofCanvas(Container viewport, boolean scrolled, int linethickness) {
        super(viewport, scrolled);
        setlinethickness(linethickness);
    }

    protected void claimFocus() {
        getProofWindow().claimDisproofFocus();
    }

    public String getSelections(String sep) {
        Alert.abort("DisproofCanvas.getSelections");
        return ""; // shut up compiler
    }
    
    public String getTextSelections(String sep) {
        Alert.abort("DisproofCanvas.getTextSelections");
        return ""; // shut up compiler
    }

    public void setSequentBox(int width, int ascent, int descent) {
        removeAll(); // seems to be necessary
        setOrigin(0, -ascent);
    }

    public Dimension getPreferredSize() {
        return getSize();
    }

    public EmphasisableItem findEmphasisable(int x, int y) {
        int nc = child.getComponentCount(); // oh dear ...
        for (int i=0; i<nc; i++) {
            Component c = child.getComponent(i); // oh dear ...
            if (c instanceof EmphasisableItem &&
                ((EmphasisableItem)c).idX==x && ((EmphasisableItem)c).idY==y)
                return (EmphasisableItem)c;
        }
        return null;
    }
}
