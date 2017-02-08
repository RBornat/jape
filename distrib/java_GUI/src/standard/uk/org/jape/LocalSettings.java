/*
    Copyright © 2003-17 Richard Bornat & Bernard Sufrin
     
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
// which copy of LocalSettings to link into the mix. But Solaris, Linux and Windows are all 
// so similar that they share this file. Mac OS X is dealt with in 
// distrib/MacOSX/java_GUI/LocalSettings.java. 
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
    
    public static final String howToFormulaSelect =
	"Formula selection is done with a single left-button click of the mouse.";
    
    public static final String howToTextSelect =
	"Subformula selection is done by pressing and dragging " +
	"with the mouse's middle button " +
	"(if you haven't got a middle button, hold down the " +
	"Alt key and use the left button). " +
	"You can modify an existing selection if you hold down the shift " +
	"key. The Control key (CTRL) lets you make multiple " +
	"subformula selections.";
    
    public static final String howToDragFormulae =
	"You drag a draggable (blue box) formula by pressing " +
	"(not clicking) the mouse's left button over it, holding "+
	"still for a brief interval, " +
	"and then moving the mouse while still holding the button down.";
    
    public static final String howToDragDisproofStuff =
	"You drag a thing by pressing (not clicking) the mouse's left button " +
	"over it, holding still for a brief interval, " +
	"and then moving the mouse while still holding its button down. " + 
	"If you use the middle button, you drag a duplicate copy " +
	"(if you haven't got a middle button, hold down the Alt key and use " +
	"the left button).";
}



