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
                                 implements Viewportable, SelectionConstants {

    protected JapeCanvas() {
        super();
        addMouseListener(new MouseAdapter() {
            public void mouseClicked(MouseEvent e) {
                click(e);
            }
        });
    }

    // I really must organise a 'selection halo' round items ...
    protected void click(MouseEvent e) {
        switch (LocalSettings.mouseDownKind(e)) {
            case TextSelection:
            case ExtendedTextSelection:
                killTextSelections(null);
                break;
            case DisjointTextSelection:
            case ExtendedDisjointTextSelection:
                break;
            case Selection:
                killSelections((byte)0xFF);
                break;
            case ExtendedSelection:
            case DisjointSelection:
            case ExtendedDisjointSelection:
                break;
            default:
                Alert.abort("JapeCanvas.click eventKind="+LocalSettings.mouseDownKind(e));
        }
    }
    
    protected abstract void declareSelection(byte eventKind, byte selkind);

    protected abstract void declareTextSelection(SelectableTextItem item, byte eventKind);

    protected void killSelections(byte selmask) {
        Component[] cs = child.getComponents(); // oh dear ...
        for (int i=0; i<cs.length; i++) {
            if (cs[i] instanceof SelectableTextItem) {
                SelectableTextItem sti = (SelectableTextItem)cs[i];
                if (sti.selectionRect!=null && (sti.selectionRect.selkind & selmask)!=0)
                    sti.select(NoSel);
            }
        }
    }

    protected void killTextSelections(SelectableTextItem leave) {
        int nc = child.getComponentCount();
        for (int i=0; i<nc; i++) {
            Component c = child.getComponent(i);
            if (c instanceof SelectableTextItem && c!=leave)
                ((SelectableTextItem)c).removeTextSelections();
        }
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
