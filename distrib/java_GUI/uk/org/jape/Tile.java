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

import javax.swing.border.Border;
import javax.swing.BorderFactory;
import java.awt.Component;
import javax.swing.JLabel;
import java.awt.event.MouseEvent;
import javax.swing.event.MouseInputAdapter;

public class Tile extends JLabel {
    final String text;
    
    static final int spacing = LocalSettings.TileSpacing;
    static final Border padding = BorderFactory.createEmptyBorder(spacing/2,spacing,spacing/2,spacing),
                        raisedbevel = BorderFactory.createRaisedBevelBorder(),
                        loweredbevel = BorderFactory.createLoweredBevelBorder(),
                        compoundbevel = BorderFactory.createCompoundBorder(raisedbevel, loweredbevel),
                        border = BorderFactory.createCompoundBorder(compoundbevel, padding);

    public Tile(final String text) {
        super(text); this.text = text;

        setFont(JapeFont.getFont(ProtocolConstants.termFontNum));

        setBorder(border);
        MouseInteractionListener mil = new MouseInteractionAdapter() {
            public void doubleclicked(byte eventKind, MouseEvent e) {
                Reply.sendCOMMAND("tileact \""+JapeFont.toAscii(text)+"\"");
            }
            public void pressed(byte eventKind, MouseEvent e) {
                System.err.println("mouse pressed "+e.getX()+","+e.getY()+" insets="+getInsets());
                Component c = Tile.this;
                while (c!=null) {
                    System.err.println(c+"\n");
                    c = c.getParent();
                }
            }
        };
        addMouseListener(mil);
        addMouseMotionListener(mil);
    }
}
