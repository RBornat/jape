/*
    $Id$
    
    Copyright © 2003 Richard Bornat & Bernard Sufrin
     
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
import java.util.Properties;
import java.io.FileInputStream;

public class LocalSettings implements SelectionConstants {

    static boolean debug = null!=System.getProperty("debug.settings");

    private static Properties props = new Properties();

    static {
        String  config = System.getProperty("jape.settings");
        if (config!=null)
        try { // generalize me!
            props.load(new FileInputStream(config));
            if (debug) System.err.println("Using ./jape.properties as configuration");
            if (debug) props.list(System.err);
        }
        catch (Exception e) {
            System.err.println(e + "(while loading settings)\n[Falling back to default configuration]");
        }
    }

    static private byte getProp(String name, int defaultvalue) {
        String val   = props.getProperty(name);
        byte   value = (byte) defaultvalue;
        if (val!=null) 
        try
        {   value = Byte.decode(val).byteValue();
        }
        catch (Exception e)
        {  // should go to GUI, but we mayn't have one yet!
            System.err.println(e + " property name: " + name);
        }
        if (debug) System.err.println(name + " = " + value);
        return value;
    }
    
    static private String getProp(String name, String defaultvalue) {
        String val   = props.getProperty(name);
        String value = val==null?defaultvalue:val;
        if (debug) System.err.println(name + " = " + value);
        return value;
    }
    

    // focus in panel windows

    public static final boolean showPanelWindowFocus = false;
    
    // how to set up menus
    
    public static final boolean panelWindowMenus = false;
    
    public static final boolean aboutMenuItemNeeded = true,
                                quitMenuItemNeeded  = true,
                                prefsMenuItemNeeded = true;

    public static final boolean windowMenuItemsTicked = false;
    
    // size of windows
    
    public static final Dimension DefaultProofWindowSize = new Dimension(625, 500);
    public static final int PosIncr = 50;

    // size of fonts
    
    public static final byte 	FormulaFontSize     = getProp("font.formula.size",     18),
                                defaultOtherSize    = getProp("font.other.size",       14),
                                ReasonFontSize      = getProp("font.reason.size",      defaultOtherSize),
                                ProvisoFontSize     = getProp("font.proviso.size",     defaultOtherSize),
                                PanelButtonFontSize = getProp("font.panelbutton.size", defaultOtherSize),
                                PanelEntryFontSize  = getProp("font.panelentry.size",  defaultOtherSize);

    public static final String  fontStyle           = getProp("fonts.family", "sanserif");
    
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
    
    // how to mark an entry in a conjecture panel
    
    public static final String tick = "\u2713", cross = "\u2717";

    // window titling
    
    public static final boolean UnicodeWindowTitles = false;

    public static void main(String[] arg)
    { 
      new LocalSettings();
    }
}



