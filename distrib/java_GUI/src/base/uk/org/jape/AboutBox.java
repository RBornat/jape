// needs GPL info in the dialog.

/* 
    $Id$

    Copyright © 2003-12 Richard Bornat & Bernard Sufrin
     
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
    protected static String engineversion = null, 
                            nameversion   = "$Name$",
                            dateversion   = "$Date$";
    
    // for the moment we do something dead simple ...
    public static void showAboutBox() {
        String guiversion = getGUIVersion();
        Alert.showAlert(Alert.Plain, 
            "This is the platform-independent GUI for the Jape proof calculator"+
            (guiversion==null ? "" : ",\nversion "+guiversion)+
            (engineversion==null ? "" : 
             (guiversion==null ? ",\n" : ", ") + "working with Jape proof engine version "+engineversion)+
            ".\n\n"+
            
            "Jape engine and GUI copyright © 2003-14 Richard Bornat & Bernard Sufrin.\n\n"+
            
            "Jape is free software; you can redistribute it and/or modify it under\n"+
            "the terms of the GNU General Public License as published by the Free\n"+
            "Software Foundation; either version 2 of the License, or (at your option)\n"+
            "any later version.\n\n"+
                        
            "Jape is distributed in the hope that it will be useful, but WITHOUT ANY\n"+
            "WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS\n"+
            "FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.\n\n"+
                        
            "You should have received a copy of the GNU General Public License with Jape;\n"+
            "if not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,\n"+
            "Boston, MA  02111-1307, USA (or visit http://www.gnu.org)."
                        );
    }
    
    public static void setVersion(String _version) {
        engineversion = _version;
    }
    
    private static String parseVersion(String s) {
        int n = s.length();
        int i = s.indexOf(' ');
        if (i==-1) return null;
        
        i++;
        while (i<n) {
            char c=s.charAt(i);
            if (c==' ' || 'a'<=c && c<='z' || 'A'<=c&&c<='z' || c=='_' || c=='$') i++;
            else break;
        }
        if (i==n) return null;
        
        int j=i;
        while (j<n) {
            char c=s.charAt(j);
            if (c==' ' || c=='$' ) break;
            else j++;
        }
        
        return s.substring(i,j).replace('_','.');
    }
    
    private static String getGUIVersion() {
        String guiversion = parseVersion(nameversion);
        return guiversion==null?  parseVersion(dateversion) : guiversion;
    }
    
    public static String getVersion() {
        String guiversion = getGUIVersion();
        
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



