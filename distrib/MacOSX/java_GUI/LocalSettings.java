/* 
    $Id$

    Copyright © 2003-5 Richard Bornat & Bernard Sufrin
     
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

package uk.org.jape;

// the MacOSX version of LocalSettings.

import com.apple.eawt.Application;
import com.apple.eawt.ApplicationAdapter;
import com.apple.eawt.ApplicationEvent;

import com.apple.mrj.MRJFileUtils;

import java.awt.Dimension;

import java.awt.event.MouseEvent;

import java.lang.IllegalStateException;

import java.io.File;

public class LocalSettings implements SelectionConstants {
    // focus in panel windows
                                          
    public static final boolean showPanelWindowFocus = true;
                                          
    // how to set up menus
                                          
    public static final boolean panelWindowMenus      = true,
                                aboutMenuItemNeeded   = false,
                                quitMenuItemNeeded    = false,
                                prefsMenuItemNeeded   = false,
                                windowMenuItemsTicked = true,
                                hideSurrogateWindow   = true;

    // size of windows
    
    public static final Dimension DefaultProofWindowSize = new Dimension(450, 350);
    public static final int PosIncr = 35;

    // size of fonts -- these optimised for Lucida Sans Unicode (blush)
    
    public static final byte 	FormulaFontSize     = 18,
                                NonFormulaFontSize  = 14;

    public static final String fontStyle = "sanserif";

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
    
    public static byte mousePressWorldItemMeans(MouseEvent e) {
        return e.isAltDown() ? NewWorldDrag : MoveWorldDrag;
    }

    public static byte mouseClickWorldItemMeans(MouseEvent e) {
        return !e.isAltDown() && !e.isShiftDown() &&
               !e.isMetaDown() && !e.isControlDown() ? WorldClick : WorldNoClick;
    }

    // and on a WorldLabel
    
    public static byte mousePressWorldLabelMeans(MouseEvent e) {
	return e.isAltDown() ? NewLabelDrag : MoveLabelDrag;
    }
    
    // how to mark an entry in a conjecture panel
    
    public static final String tick = "\u221A", cross = "\u2022";
            // actually square root, blob in Konstanz

    // window titling (until we have proper Unicode operator coverage, esp. turnstile)
    
    public static final boolean UnicodeWindowTitles = true; // now that I have Lucida Sans Unicode

    /* ************************ MacOS specific bits ************************ */

    public LocalSettings() {
        Application appl = new Application();
        appl.addApplicationListener(new ApplicationAdapter() {
            public void handleAbout(ApplicationEvent evt) {
                Jape.handleAbout();
            }
            public void handleOpenFile(ApplicationEvent evt) {
                JapeMenu.doOpenFile(evt.getFilename());
            }
            public void handlePreferences(ApplicationEvent evt) {
                Jape.handlePrefs();
            }
            public void handleQuit(ApplicationEvent evt) {
                Jape.handleQuit();
                // Jape.crash("The engine isn't responding!"); // if we return from handleQuit, we didn't exit
            }
        });
    }
}
