/* 
    $Id$
    
    Copyright © 2003-4 Richard Bornat & Bernard Sufrin
        
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
    // not yet efficient
    public String getSelections(String sep) {
        String s = null;
        int nc = child.getComponentCount(); // oh dear ...
        for (int i=0; i<nc; i++) {
            String s1 = getSelection(child.getComponent(i)); // oh dear ...
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
        if (c instanceof SelectableProofItem && ((SelectableProofItem)c).getSelected()) {
            SelectableProofItem item = (SelectableProofItem)c;
            return (item.idX+" "+item.idY+" "+
                    protocolSelClass("getSelection", item.getSelectionKind()));
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

    protected void notifySelectionChange(DisplayItem item) {
        if (item!=null && item.getSelected())
            notifySelect(item);
        else
            notifyDeselect();
    }
    
    protected byte protocolSelClass(String id, byte selclass) {
        switch (selclass) {
            case HypSel   : return HypTextItem;
            case ConcSel  : return ConcTextItem;
            case ReasonSel: return ReasonTextItem;
            default       : Alert.abort("ProofCanvas."+id+" selkind="+selclass);
                            return PunctTextItem; // shut up compiler
        }
    }
    
    public void killSelections(byte mask) {
        Component[] cs = child.getComponents(); // oh dear ...
        for (int i=0; i<cs.length; i++) {
            if (cs[i] instanceof SelectableProofItem &&
                (((SelectableProofItem)cs[i]).getSelectionKind() & mask)!=0) {
                ((SelectableProofItem)cs[i]).setSelected(false);
            }
        }
    }

    protected void doSelectAction(DisplayItem di) {
        if (di instanceof SelectableProofItem) {
            SelectableProofItem item = (SelectableProofItem) di;
            switch (item.getSelectionKind()) {
                case ReasonSel:
                    killAllSelections();
                    break;
                case HypSel:
                    killSelections((byte)(HypSel | ReasonSel));
                    break;
                case ConcSel:
                    killSelections((byte)(ConcSel | ReasonSel));
                    break;
                default:
                    Alert.abort("ProofCanvas.doSelectAction("+di+");");
            }
        }
    }

    protected void doExtendSelectAction(DisplayItem di) {
        if (di instanceof SelectableProofItem) {
            SelectableProofItem item = (SelectableProofItem) di;
            switch (item.getSelectionKind()) {
                case ReasonSel:
                case ConcSel: // only one at a time
                    doSelectAction(di);
                    break;
                case HypSel: // several allowed
                    break;
                default:
                    Alert.abort("ProofCanvas.doExtendSelectAction("+di+");");
            }
        }
    }

    protected void doHitAction(DisplayItem di) {
        if (di instanceof SelectableProofItem)
            Reply.send("ACT "+di.idX+" "+di.idY+" "+
                       protocolSelClass("notifyHit",
                                    ((SelectableProofItem)di).getSelectionKind()));
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
