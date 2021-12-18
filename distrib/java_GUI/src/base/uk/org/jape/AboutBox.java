/* 
    Copyright © 2003-19 Richard Bornat & Bernard Sufrin
     
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

public class AboutBox {
    protected static String engineversion = null;
    
    // for the moment we do something dead simple ...
    public static void showAboutBox() {
        String guiversion = Version.guiversion;
        Alert.showAlert(Alert.Plain, 
            "This is the platform-independent GUI for the Jape proof calculator"+
            (guiversion==null ? "" : ",\nversion "+guiversion)+
            (engineversion==null ? "" : 
             (guiversion==null ? ",\n" : ", ") + "working with Jape proof engine version "+engineversion)+
            ".\n\n"+
            
            "Jape engine and GUI copyright © 2003-19 Richard Bornat & Bernard Sufrin.\n\n"+
            
            "Jape is free software; you can redistribute it and/or modify it under\n"+
            "the terms of the GNU General Public License as published by the Free\n"+
            "Software Foundation; either version 2 of the License, or (at your option)\n"+
            "any later version.\n\n"+
                        
            "Jape is distributed in the hope that it will be useful, but WITHOUT ANY\n"+
            "WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS\n"+
            "FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.\n\n"+
                        
            "You should have received a copy of the GNU General Public License with Jape;\n"+
            "if not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,\n"+
            "Boston, MA  02111-1307, USA (or visit http://www.gnu.org)."+
            
            (Jape.onMacOSX ? 
                    "\n\n"+
                    "macOS Jape.app is assembled using code that derives from:\n"+
                    "    jarbundler 3.3.0\n"+
                    "        Copyright © 2015, UltraMixer Digital Audio Solutions <info@ultramixer.com>\n"+
                    "              and Seth J. Morabito <sethm@loomcom.com>,\n"+
                    "        licensed under Apache License v2.0\n"
             : ""
            )+
            
            (Jape.inFlatpak ? 
                    "\n\n"+
                    "The Flatpak distribution of Jape is assembled using code initially developed\n"+
                    "by Timo Triebensky <timo@binsky.org>, taken from his repo at \n"+
                    "    https://github.com/binsky08/jape-flatpak-builder.git\n"
             : ""
            )

                        );
    }
    
    public static void setEngineVersion(String version) {
        engineversion = version;
    }
    
    public static String getVersion() {
        String guiversion = Version.guiversion;
        
        if (engineversion!=null && guiversion!=null)
            return engineversion.equals(guiversion) ? engineversion : 
                "(engine) "+engineversion+"; (GUI) "+guiversion;
        else
        if (engineversion==null) 
            return "(GUI) "+guiversion;
        else
            return "(engine) "+engineversion;
    }
}





