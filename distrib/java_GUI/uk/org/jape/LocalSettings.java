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

// this class, which can be replaced by a platform-specific version to aid 
// look-and-feel localisation (see for example jape/MacOSX/java_japeserver/LocalSettings.java)
// or I don't know what else.

import java.awt.Dimension;
import java.awt.event.MouseEvent;

public class LocalSettings implements SelectionConstants {
	
    // how to set up menus
    public static final boolean panelWindowMenus = false;
    
    public static final boolean aboutMenuItemNeeded = true,
                                quitMenuItemNeeded  = true,
                                prefsMenuItemNeeded = true;

    // size of windows
    public static final Dimension DefaultProofWindowSize = new Dimension(625, 500);
    public static final int PosIncr = 50;

    // size of fonts
    public static final byte 	FormulaFontSize     = 18,
                                ReasonFontSize      = 14,
                                ProvisoFontSize     = 14,
                                PanelButtonFontSize = 14,
                                PanelEntryFontSize  = 14;

    // spacing of tiles
    public static final int TileSpacing = 7;

    // what a mouseDown means (this stands until people tell me better)
    public static byte mouseDownKind(MouseEvent e) {
        byte kind = e.isAltDown() ? TextSelMask : PureSelMask;
        if (e.isShiftDown()) kind |= ExtendedSelMask;
        if (e.isMetaDown())  kind |= DisjointSelMask;
        return kind;
    }

    // how to mark an entry in a conjecture panel
    public static final String tick = "\u2713", cross = "\u2717";
}
