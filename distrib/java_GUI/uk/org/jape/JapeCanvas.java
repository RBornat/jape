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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.Rectangle;
import java.awt.Point;

public abstract class JapeCanvas extends ContainerWithOrigin
                                 implements Viewportable,
                                            SelectionConstants {

    protected JapeCanvas() {
        super();
        addMouseListener(new MouseInteractionAdapter() {
            public void clicked(byte eventKind, MouseEvent e) {
               JapeCanvas.this.clicked(eventKind, e);
            }
            public void textreleased(byte eventKind, boolean isClick, MouseEvent e) {
                JapeCanvas.this.textreleased(eventKind, isClick, e);
            }
        });
    }

    // linethickness is a canvas-wide property
    protected int linethickness;

    // from it we derive the selection halo for text items
    protected int getSelectionHalo() {
        return 2*linethickness;
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
    protected void textreleased(byte eventKind, boolean isClick, MouseEvent e) {
        if (isClick) {
            switch (eventKind) {
                case TextSelection:
                case ExtendedTextSelection:
                    killTextSelections(null);
                    break;
                case DisjointTextSelection:
                case ExtendedDisjointTextSelection:
                    break;
                default:
                    Alert.abort("JapeCanvas.textreleased eventKind="+eventKind);
            }
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

    Container viewport = null;
    public void inViewport(Container vp) { viewport = vp; }
    
    // when we are in a viewport, we get the mouse events.
    public boolean contains(int x, int y) {
        return viewport!=null || super.contains(x,y);
    }

    // when we are in a viewport, we tell you what the viewport sees
    public Rectangle viewGeometry() {
        if (viewport==null)
            return getBounds();
        else {
            Rectangle v = viewport.getBounds();
            v.x -= (getX()+child.getX()); v.y -= (getY()+child.getY()); // oh dear ...
            return v;
        }
    }

    public void clearPane() {
        removeAll(); repaint();
        if (viewport!=null)
            viewport.validate();
    }
}
