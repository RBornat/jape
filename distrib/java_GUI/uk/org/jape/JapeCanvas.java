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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public abstract class JapeCanvas extends ContainerWithOrigin {

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
}
