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

public class ProofCanvas extends JapeCanvas implements ProtocolConstants, SelectionConstants {

    public ProofCanvas(Container viewport, boolean scrolled) { super(viewport, scrolled); }

    public byte proofStyle;

    protected void claimFocus() {
        getProofWindow().claimProofFocus();
    }

    // these are not yet coming out in time order ...
    // reply always ends with a blank line
    // not yet efficient
    public String getSelections(String sep) {
        String s = null;
        int nc = child.getComponentCount(); // oh dear ...
        for (int i=0; i<nc; i++) {
            String s1 =getSelection(child.getComponent(i)); // oh dear ...
            if (s1!=null) {
                if (s==null)
                    s=s1;
                else
                    s=s+sep+s1;
            }
        }
        return s; 
    }

    protected String getSelection(Component c) {
        byte selclass;
        if (c instanceof DisplayItem && c instanceof SelectableItem &&
            (selclass = protocolSelClass("getSelection", (SelectableItem)c))!=PunctTextItem) {
            return ((DisplayItem)c).idX+" "+((DisplayItem)c).idY+" "+selclass;
        }
        else
            return null;
    }
    
    protected void notifySelect(DisplayItem d) {
        String s = "SELECT "+getSelection(d);
        int nc = child.getComponentCount(); // oh dear ...
        for (int i=0; i<nc; i++) {
            Component c = child.getComponent(i); // oh dear ...
            if (c!=d) {
                String s1 = getSelection(c);
                if (s1!=null)
                    s = s+" "+s1;
            }
        }
        Reply.send(s);
    }

    protected void notifyDeselect() {
        String s = getSelections(" ");
        Reply.send("DESELECT"+(s==null ? "" : " "+s));
    }

    protected byte protocolSelClass(String id, SelectableItem si) {
        switch (si.getSelkind()) {
            case NoSel    : return PunctTextItem;
            case HypSel   : return HypTextItem;
            case ConcSel  : return ConcTextItem;
            case ReasonSel: return ReasonTextItem;
            default       : Alert.abort("ProofCanvas."+id+" selkind="+
                                        si.getSelkind());
                            return PunctTextItem; // shut up compiler
        }
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
                    s1 = sti.idX+Reply.stringSep+sti.idY+Reply.stringSep+s1;
                    if (s==null)
                        s=s1;
                    else
                        s=s+sep+s1;
                }
            }
        }
        return s;
    }

    protected void notifyHit(DisplayItem di) {
        if (di instanceof SelectableItem)
            Reply.send("ACT "+di.idX+" "+di.idY+" "+
                       protocolSelClass("notifyHit",(SelectableItem)di));
        else
            Alert.abort("ProofCanvas.notifyHit di="+di);
    }

    public SelectableProofItem findSelectable(int x, int y) {
        int nc = child.getComponentCount(); // oh dear ...
        for (int i=0; i<nc; i++) {
            Component c = child.getComponent(i); // oh dear ...
            if (c instanceof SelectableProofItem &&
                ((SelectableProofItem)c).idX==x && ((SelectableProofItem)c).idY==y)
                return (SelectableProofItem)c;
        }
        return null;
    }
}
