// needs GPL info in the dialog.

/* 
    $Id$

    Copyright © 2003-4 Richard Bornat & Bernard Sufrin
     
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

public class AboutBox {
    protected static String version = null;
    
    // for the moment we do something dead simple ...
    public static void showAboutBox() {
        Alert.showAlert(Alert.Plain, 
            "This is the platform-independent GUI for the Jape proof calculator"+
                (version==null ? "" : ", working with "+version)+
                ". Jape engine and GUI copyright © 2003 Richard Bornat & Bernard Sufrin.");
        /*
            I should give access to this text:

            Jape is free software; you can redistribute it and/or
            modify it under the terms of the GNU General Public License
            as published by the Free Software Foundation; either version 2
            of the License, or (at your option) any later version.
            
            Jape is distributed in the hope that it will be useful,
            but WITHOUT ANY WARRANTY; without even the implied warranty of
            MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
            GNU General Public License for more details.
            
            You should have received a copy of the GNU General Public License
            along with Jape; if not, write to the Free Software
            Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA,
            or look at http://www.gnu.org.

        */
    }
	
    public static void setVersion(String _version) {
        version = _version;
    }
	
}