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

import java.awt.Rectangle;

public class DisproofCanvas extends JapeCanvas implements ProtocolConstants, SelectionConstants {
    public String getSelections(String sep) {
        Alert.abort("DisproofCanvas.getSelections");
        return ""; // shut up compiler
    }
    public String getTextSelections(String sep) {
        Alert.abort("DisproofCanvas.getTextSelections");
        return ""; // shut up compiler
    }

    // somehow or other this tells me what the coordinates mean ...
    
    protected Rectangle sequentBounds;
    
    public void setSequentBox(int x, int y, int width, int height) {
        sequentBounds = new Rectangle(x, y, width, height);
    }
}
