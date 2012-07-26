/* 
    $Id$

    Copyright © 2003-8 Richard Bornat & Bernard Sufrin
     
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

import java.awt.Dimension;

import java.awt.event.MouseEvent;

@SuppressWarnings("deprecation") // really? RB 22/03/2012
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
    
    public static final byte    FormulaFontSize     = 18,
                                NonFormulaFontSize  = 14;

    // public static final String fontStyle = "sanserif";

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
    
    public static final String tick = "\u2713", cross = "\u2717";
    
    public static final boolean UnicodeWindowTitles = true; // now that I have good Unicode fonts

    /* ************************ MacOS specific bits ************************ */

    public LocalSettings() { }
    static {
        Application appl = Application.getApplication();
        appl.addApplicationListener(new ApplicationAdapter() {
            public void handleAbout(ApplicationEvent evt) {
                System.err.println("JAPE ABOUT: ");
                Jape.handleAbout();
            }
            
            public void handleOpenFile(ApplicationEvent evt) {
                System.err.println("JAPE OPEN FILE: "+evt.getFilename());
                JapeMenu.doOpenFile(evt.getFilename());
            }
            
            public void handleOpenApplication(ApplicationEvent evt) {
                System.err.println("JAPE OPEN APPLICATION: ");
            }

            public void handlePreferences(ApplicationEvent evt) {
                System.err.println("JAPE PREFS: ");
                Jape.handlePrefs();
            }
            public void handleQuit(ApplicationEvent evt) {
                System.err.println("JAPE CLOSE APPLICATION: ");
                Jape.handleQuit();
                // Jape.crash("The engine isn't responding!"); // if we return from handleQuit, we didn't exit
            }
        });
    }
    
    public static final String howToFormulaSelect =
        "Formula selection on Mac OS X is done with a single click " +
        "(with a two- or three-button mouse, it's a left-button click).";
    
    public static final String howToTextSelect =
        "Subformula selection on Mac OS X is done by holding down the " +
        "alt (option) key while pressing and dragging over a formula. " +
        "You can modify an existing selection by holding down the shift " +
        "key. The command key (apple, propellor) lets you make multiple " +
        "subformula selections." +
        "\n\n" +
        "(With a two-button mouse you subformula-select with the left " +
        "button plus the alt (option) key. With a three-button mouse you " +
        "use the middle button.)";
    
    public static final String howToDragFormulae =
        "On Mac OS X you drag a draggable (blue box) formula by pressing " +
        "(not clicking) the mouse over it, holding still for a brief interval, " +
        "and then moving the mouse while still holding its button down. " + 
        "(With a two- or three-button mouse, use the left button.)";
    
    public static final String howToDragDisproofStuff =
        "On Mac OS X you drag a thing by pressing " +
        "(not clicking) the mouse over it, holding still for a brief interval, " +
        "and then moving the mouse while still holding its button down. " + 
        "(With a two- or three-button mouse, use the left button.) If you " +
        "hold down the alt (option) key throughout the gesture, you drag a " +
        "duplicate copy.";
}

