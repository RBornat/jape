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
import java.awt.event.MouseEvent;

public class ProofCanvas extends JapeCanvas implements ProtocolConstants, SelectionConstants {

    public ProofCanvas() { super(); }

    // DisplayItems (things that have an identity) get added at the front;
    // other items (lines, rects) at the back.
    public Component add(Component c) {
        if (c instanceof DisplayItem)
            super.add(c, 0);
        else
            super.add(c);
        return c;
    }

    public byte proofStyle;
    
    // these are not yet coming out in time order ...
    // reply always ends with a blank line
    // not yet efficient
    public String getSelections() {
        String s = null;
        int nc = child.getComponentCount(); // oh dear ...
        for (int i=0; i<nc; i++) {
            Component c = child.getComponent(i); // oh dear ...
            if (c instanceof DisplayItem && c instanceof SelectableItem) {
                byte selclass;
                switch (((SelectableItem)c).getSelkind()) {
                    case NoSel    : continue;
                    case HypSel   : selclass = HypTextItem; break;
                    case ConcSel  : selclass = ConcTextItem; break;
                    case ReasonSel: selclass = ReasonTextItem; break;
                    default       : Alert.abort("ProofCanvas.getSelections selkind="+
                                                ((SelectableItem)c).getSelkind());
                                    selclass=NoSel; // shut up compiler
                }
                String s1 = ((DisplayItem)c).idX+" "+((DisplayItem)c).idY+" "+selclass+"\n";
                if (s==null)
                    s=s1;
                else
                    s=s+s1;
            }
        }
        return s==null ? "" : s; 
    }

    // not efficient, not in time order
    // always ends with a blank line
    public String getTextSelections() {
        String s = null;
        int nc = child.getComponentCount(); // oh dear ...
        for (int i=0; i<nc; i++) {
            Component c = child.getComponent(i); // oh dear ...
            if (c instanceof TextSelectableItem) {
                TextSelectableItem sti = (TextSelectableItem)c;
                String s1 = sti.getTextSelections();
                if (s1!=null) {
                    s1 = sti.idX+Reply.stringSep+sti.idY+Reply.stringSep+s1+"\n";
                    if (s==null)
                        s=s1;
                    else
                        s=s+s1;
                }
            }
        }
        return s==null ? "" : s;
    }
}
