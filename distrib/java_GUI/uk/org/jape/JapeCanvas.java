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

public abstract class JapeCanvas extends ContainerWithOrigin implements Viewportable {

    static final byte NoSel = 0;

    protected JapeCanvas() {
        super();
        addMouseListener(new MouseAdapter() {
            public void mouseClicked(MouseEvent e) {
                click(e);
            }
        });
    }

    protected void click(MouseEvent e) {
        killSelections((byte)0xFF);
    }
    
    protected abstract void selectionMade(SelectableTextItem item, MouseEvent e, byte selkind);

    protected abstract void textselectionMade(SelectableTextItem item, MouseEvent e);

    protected void killSelections(byte selmask) {
        int lim = child.getComponentCount();
        for (int i=0; i<lim; i++) {
            Component c = child.getComponent(i);
            if (c instanceof SelectableTextItem && (((SelectableTextItem)c).selected & selmask) != 0)
                ((SelectableTextItem)c).select(NoSel);
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
            v.x -= (getX()+child.getX()); v.y -= (getY()+child.getY());
            return v;
        }
    }

    public void clearPane() {
        child.removeAll(); repaint();
        if (viewport!=null)
            viewport.validate();
    }
}
