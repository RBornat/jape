/* 
    $Id$

    Copyright © 2003 Richard Bornat & Bernard Sufrin
     
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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.Rectangle;
import java.awt.Point;

import java.lang.reflect.Method;

public abstract class JapeCanvas extends ContainerWithOrigin
                                 implements SelectionConstants {

    protected JapeCanvas(Container viewport, boolean scrolled) {
        super(viewport, scrolled);
        addMouseListener(new JapeMouseTextAdapter() {
            public void clicked(byte eventKind, MouseEvent e) {
                JapeCanvas.this.claimFocus();
                JapeCanvas.this.clicked(eventKind, e);
            }
            public void textreleased(byte eventKind, boolean isClick, MouseEvent e) {
                if (isClick)
                    JapeCanvas.this.textclicked(eventKind, e);
            }
        });
    }

    protected abstract void claimFocus();
                                     
    // DisplayItems (things that have an identity) get added at the front;
    // other items (lines, rects, etc.) at the back.
    public Component add(Component c) {
        if (c instanceof DisplayItem)
            super.add(c, 0);
        else
            super.add(c);
        return c;
    }

    // linethickness is a canvas-wide property
    protected int linethickness;

    // if linethickness changes, tell anybody who has a setlinethickness method
    public void setlinethickness(int linethickness) {
        this.linethickness = linethickness;
        int nc = child.getComponentCount();
        if (nc>0) {
            Class [] params = new Class[] { Integer.class };
            Object [] args = new Object[] { new Integer(linethickness) };
            for (int i=0; i<nc; i++) {
                Component c = child.getComponent(i);
                Class cl = c.getClass();
                try {
                    Method m = cl.getMethod("setlinethickness", params);
                    try {
                        m.invoke(c, args);
                    } catch (java.lang.IllegalAccessException e) {
                        Logger.log.println("private setlinethickness in "+c);
                    } catch (Exception e) {
                        Logger.log.println("setlinethickness invocation: "+c+"; => "+e);
                    } 
                } catch (java.lang.NoSuchMethodException e) { }
            }
        }
    }

    // from it we derive the selection halo for text items
                                     
    protected int getSelectionHalo() {
        return 2*linethickness;
    }

    // and the surround gap for selection rectangles, emphasis and I don't know what else
    protected int getSurroundGap() {
        return linethickness*3/2;
    }
    
    // click on canvas kills selections
    protected void clicked(byte eventKind, MouseEvent e) {
        switch (eventKind) {
            case Selection:
                killSelections((byte)0xFF);
                Reply.send("DESELECT");
                break;
            case ExtendedSelection:
            case DisjointSelection:
            case ExtendedDisjointSelection:
                break;
            default:
                Alert.abort("JapeCanvas.click eventKind="+eventKind);
        }
    }

    // text click on canvas kills text selections
    protected void textclicked(byte eventKind, MouseEvent e) {
        switch (eventKind) {
            case TextSelection:
            case ExtendedTextSelection:
                killTextSelections(null);
                break;
            case DisjointTextSelection:
            case ExtendedDisjointTextSelection:
                break;
            default:
                Alert.abort("JapeCanvas.textclicked eventKind="+eventKind);
        }
    }

    public abstract String getSelections(String sep);
    
    protected void killSelections(byte selmask) {
        Component[] cs = child.getComponents(); // oh dear ...
        for (int i=0; i<cs.length; i++) {
            if (cs[i] instanceof SelectableItem) {
                SelectableItem s = (SelectableItem)cs[i];
                if (s.selkindOverlaps(selmask))
                    s.deselect();
            }
        }
    }

    public abstract String getTextSelections(String sep);
    
    protected void killTextSelections(TextSelectableItem leave) {
        Component[] cs = child.getComponents(); // oh dear ...
        for (int i=0; i<cs.length; i++)
            if (cs[i] instanceof TextSelectableItem && cs[i]!=leave)
                ((TextSelectableItem)cs[i]).deTextSelect();
    }

    public int getTextSelectionCount() {
        int nc = child.getComponentCount(); // oh dear ...
        int count = 0;
        for (int i=0; i<nc; i++) {
            Component c = child.getComponent(i);
            if (c instanceof TextSelectableItem)
                count += ((TextSelectableItem)c).getTextSelectionCount();
        }
        return count;
    }

    public ProofWindow getProofWindow() {
        Container c = getParent();
        while (c!=null && !(c instanceof ProofWindow))
            c = c.getParent();
        return (ProofWindow)c; // null if we ain't in a ProofWindow, obviously
    }
}
