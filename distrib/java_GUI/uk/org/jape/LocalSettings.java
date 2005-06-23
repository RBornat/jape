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

// The Jape GUI is compiled individually for each kind of OS. This isn't too much of a 
// hardship, because you have to build the engines individually. The makefile can then choose
// which copy of LocalSettings to link into the mix. There are three in the uk/org/jape directory,
// for Solaris, Linux and Windows. There's another in distrib/MacOSX/java_GUI for Mac OS X (and,
// wouldn't you know, that's the one that stops it all being done with loadClass or whatever).
// RB 22.vi.2005

import java.awt.Dimension;
import java.awt.event.MouseEvent;

public class LocalSettings implements SelectionConstants {

    // focus in panel windows

    public static final boolean showPanelWindowFocus = false;
    
    // how to set up menus
    
    public static final boolean panelWindowMenus      = false,
				aboutMenuItemNeeded   = true,
				quitMenuItemNeeded    = true,
				prefsMenuItemNeeded   = true,
				windowMenuItemsTicked = false,
				hideSurrogateWindow   = false;
    
    // size of windows
    
    public static final Dimension DefaultProofWindowSize = new Dimension(625, 500);
    public static final int PosIncr = 50;

    // size of fonts
    
    public static final byte	FormulaFontSize	    = 18,
				NonFormulaFontSize  = 14;
    
    // spacing of tiles
    public static final int TileSpacing = 7;

    // what a mouseDown means on a TextItem (I took advice from Bernard, hence isControlDown)
    
    public static byte mouseDownTextItemMeans(MouseEvent e) {
	byte kind = e.isAltDown() ? TextSelMask : PureSelMask;
	if (e.isShiftDown())    kind |= ExtendedSelMask;
	if (e.isControlDown())  kind |= DisjointSelMask;
	return kind;
    }

    // what a mouseDown means on a WorldItem (ditto)
    
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

    // window titling
    
    public static final boolean UnicodeWindowTitles = false;
}



