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

import java.awt.BorderLayout;
import javax.swing.Box;
import javax.swing.BoxLayout;
import java.awt.Container;
import javax.swing.JLabel;

public class DisproofPane extends Container {
    Container disproof;
    Box outerBox, tileCanvas;

    public DisproofPane(Container disproof) {
        super();
        this.disproof = disproof;
        setLayout(new BorderLayout());
        tileCanvas = new Box(BoxLayout.Y_AXIS);
        outerBox = new Box(BoxLayout.X_AXIS);
        outerBox.createHorizontalStrut(LocalSettings.TileSpacing);
        outerBox.add(tileCanvas);
        outerBox.createHorizontalStrut(LocalSettings.TileSpacing);
        add(disproof, BorderLayout.CENTER);
        add(outerBox, BorderLayout.EAST);
    }

    public void setTiles(String[] tiles) {
        tileCanvas.removeAll();
        tileCanvas.add(Box.createGlue());
        for (int i=0; i<tiles.length; i++) {
            tileCanvas.add(new JLabel(tiles[i]));
            if (i+1<tiles.length)
                tileCanvas.createVerticalStrut(LocalSettings.TileSpacing);
        }
        tileCanvas.getLayout().layoutContainer(tileCanvas);
        outerBox.getLayout().layoutContainer(outerBox);
        getLayout().layoutContainer(this);
        repaint();
    }
}
