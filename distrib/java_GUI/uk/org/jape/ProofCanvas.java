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

public class ProofCanvas extends JapeCanvas
                         implements ProofConstants, SelectionConstants {

    public ProofCanvas() { super(); }

    protected void declareSelection(byte eventKind, byte selkind) {
        switch (selkind) {
            case HypSel:
                // policy at present is that HypSel kills all ReasonSels,
                // and all other HypSels unless extended/disjoint
                killSelections((byte)(ReasonSel | ((eventKind&(ExtendedSelMask|DisjointSelMask))!=0 ? NoSel : HypSel)));
                break;
            case ConcSel:
                // ConcSel kills all ReasonSels and all other ConcSels
                killSelections((byte)(ReasonSel | ConcSel));
                break;
            case ReasonSel:
                // ReasonSel kills all other selections
                killSelections((byte)0xFF);
                break;
            default:
                Alert.abort("ProofCanvas.selectionMade selkind="+selkind);
        }
    }

    protected void declareTextSelection(TextItem item, byte eventKind) {
        switch (eventKind) {
            case TextSelection:
                // policy is that all text selections are killed, including current item
                killTextSelections(null);
                break;
            case ExtendedTextSelection:
                // text selections are killed in other items
                killTextSelections(item);
                break;
            case DisjointTextSelection:
            case ExtendedDisjointTextSelection:
                // nothing happens, I think
                break;
            default:
                Alert.abort("ProofCanvas.declareTextSelection eventKind="+eventKind);
        }
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
                String s1 = sti.getTextSelectionStrings();
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

    // these are not yet coming out in time order ...
    // reply always ends with a blank line
    // not yet efficient
    public String getSelections() {
        String s = null;
        int nc = child.getComponentCount(); // oh dear ...
        for (int i=0; i<nc; i++) {
            Component c = child.getComponent(i); // oh dear ...
            if (c instanceof SelectableItem) {
                byte selclass;
                SelectableItem sti = (SelectableItem)c;
                if (sti.selectionRect==null)
                    continue;
                else
                switch (sti.selectionRect.selkind) {
                    case HypSel   : selclass = HypTextItem; break;
                    case ConcSel  : selclass = ConcTextItem; break;
                    case ReasonSel: selclass = ReasonTextItem; break;
                    default       : Alert.abort("ProofCanvas.reportSelections selkind="+
                                                sti.selectionRect.selkind);
                                    selclass=0; // shut up compiler
                }
                String s1 = sti.idX+" "+sti.idY+" "+selclass+"\n";
                if (s==null)
                    s=s1;
                else
                    s=s+s1;
            }
        }
        return s==null ? "" : s; 
    }
}
