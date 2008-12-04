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

import java.awt.Color;

import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

public class JapePrefs {
    public static Color GreyTextColour		= Color.gray,
			LineColour		= Color.black,
			NoLineColour		= Color.lightGray,
			PanelBackgroundColour	= Color.white,
			ProofBackgroundColour	= Color.white,
			SelectionColour		= Color.red,
			SeparatorColour		= Color.gray,
			TextColour		= Color.black,
			TextSelectionColour	= Color.yellow,
			WorldColour		= Color.black,
                        FormulaDragHighlightColour 
                                                = Color.blue;

    public static Color OutColour	      = Color.gray,
			ForcedColour	      = Color.magenta,
			ForcedSelectionColour = Color.green;

    // I don't know if they cache this, so I will
    public static final Preferences prefs = Preferences.userNodeForPackage(JapePrefs.class);
    
    static String getProp(String key, String defaultvalue) {
	return prefs.get(key, defaultvalue);
    }
    
    static int getProp(String key, int defaultvalue) {
	String v = getProp(key, null);
	if (v==null) return defaultvalue;
	else try {
	    return Integer.parseInt(v);
	} catch (NumberFormatException e) {
	    Alert.showErrorAlert("GUI error: preference key "+JapeUtils.enQuote(key)+"\n"+
				 "returns "+JapeUtils.enQuote(v)+",\n"+
				 "which is not an integer-looking string.");
	    return defaultvalue;
	}
    }
    
    static byte getProp(String key, byte defaultvalue) {
	return (byte)getProp(key, (int)defaultvalue);
    }
    
    static void putProp(String key, String value) {
	if (value==null) prefs.remove(key);
	else prefs.put(key, value);
	flush();
    }
    
    static void putProp(String key, int value) {
	prefs.putInt(key, value);
	flush();
    }
    
    static void putProp(String key, byte value) {
	putProp(key, (int)value);
    }
    
    static void flush() {
	try {
	    prefs.flush();
	} catch (BackingStoreException e) {
	    Alert.showErrorAlert("prefs.flush got BackingStoreException "+e);
	}
    }
}
