/* 
    $Id$
    
    Copyright © 2003 Richard Bornat & Bernard Sufrin
        
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

import java.awt.Color;

import java.io.FileInputStream;

import java.util.Properties;

public class Preferences {
    public static Color GreyTextColour          = Color.gray,
                        LineColour              = Color.black,
                        NoLineColour            = Color.lightGray,
                        PanelBackgroundColour   = Color.white,
                        ProofBackgroundColour   = Color.white,
                        SelectionColour         = Color.red,
                        SeparatorColour         = Color.gray,
                        TextColour              = Color.black,
                        TextSelectionColour     = Color.yellow,
                        WorldColour             = Color.black;

    public static Color OutColour    = Color.gray,
                        ForcedColour = Color.magenta;

    private static Properties props = new Properties();

    static {
        String  config = LocalSettings.PrefsFilename;
        if (config!=null) {
            try { // generalize me!
                if (DebugConstants.preference_tracing)
                    Logger.log.println("trying to load preferences from "+config);
                props.load(new FileInputStream(config));
                if (DebugConstants.preference_tracing) {
                    Logger.log.println("Using "+config+" as configuration");
                    props.list(Logger.log);
                }
            }
            catch (Exception e) {
                Logger.log.println(e+" (while loading preferences from "+config.toString()+
                                   ")\n[Falling back to default configuration]");
            }
        }
        else {
            if (DebugConstants.preference_tracing)
                Logger.log.println("no preferences file to be found ...");
        }
    }

    static byte getProp(String name, int defaultvalue) {
        String val   = props.getProperty(name);
        byte   value = (byte) defaultvalue;
        if (val!=null) {
            try {
                value = Byte.decode(val).byteValue();
            }
            catch (Exception e) {  // should go to GUI, but we mayn't have one yet!
                Logger.log.println(e + " property name: " + name);
            }
        }
            
        if (DebugConstants.preference_tracing) Logger.log.println(name + " = " + value);
        return value;
    }

    static String getProp(String name, String defaultvalue) {
        String val   = props.getProperty(name);
        String value = val==null?defaultvalue:val;
        if (DebugConstants.preference_tracing)
            Logger.log.println(name + " = " + value);
        return value;
    }
}
