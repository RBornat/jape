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

// the MacOSX version of LocalSettings.

import com.apple.mrj.MRJAboutHandler;
import com.apple.mrj.MRJApplicationUtils;
import com.apple.mrj.MRJQuitHandler;
import com.apple.mrj.MRJPrefsHandler;

import java.awt.Dimension;

import java.awt.event.MouseEvent;

import java.lang.IllegalStateException;

public class LocalSettings implements MRJAboutHandler, MRJQuitHandler, MRJPrefsHandler,
                                      SelectionConstants {

    // how to set up menus
    public static final boolean panelWindowMenus = true;
    
    public static final boolean aboutMenuItemNeeded = false;
    public static final boolean quitMenuItemNeeded = false;
    public static final boolean prefsMenuItemNeeded = false;

    // size of windows
    public static final Dimension DefaultProofWindowSize = new Dimension(450, 350);
    public static final int PosIncr = 35;

    // size of fonts
    public static final byte 	FormulaFontSize     = 14,
                                ReasonFontSize      = 11,
                                ProvisoFontSize     = 11,
                                PanelButtonFontSize = 11,
                                PanelEntryFontSize  = 11;

    // spacing of tiles
    public static final int TileSpacing = 5;

    // what a mouseDown means on a TextItem
    public static byte mouseDownTextItemMeans(MouseEvent e) {
        byte kind = e.isAltDown() ? TextSelMask : PureSelMask;
        if (e.isShiftDown()) kind |= ExtendedSelMask;
        if (e.isMetaDown())  kind |= DisjointSelMask;
        return kind;
    }

    // what a mouseDown means on a WorldItem
    public static byte mouseDownWorldItemMeans(MouseEvent e) {
        return e.isAltDown() ? ExtendedDrag : SimpleDrag;
    }

    // how to mark an entry in a conjecture panel
    public static final String tick = "\u221A", cross = "\u2022"; // actually square root, blob in Konstanz

    /* ********************************************************************** */

    // MacOS specific bits
    public void handleAbout() {
        japeserver.handleAbout();
    }

    public void handleQuit() throws IllegalStateException {
        japeserver.handleQuit();
        throw new IllegalStateException(); // if we return from handleQuit, we didn't exit
    }
    
    public void handlePrefs() {
        japeserver.handlePrefs();
    }
    
    public LocalSettings() {
        MRJApplicationUtils.registerAboutHandler(this);
        MRJApplicationUtils.registerQuitHandler(this);
        MRJApplicationUtils.registerPrefsHandler(this);
    }
}
