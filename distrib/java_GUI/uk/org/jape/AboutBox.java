// needs GPL info in the dialog.

/* 
    $Id$

    Copyright © 2002 Richard Bornat & Bernard Sufrin
     
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

public class AboutBox {
    protected static String version = null;
    
    // for the moment we do something dead simple ...
    public static void showAboutBox() {
        Alert.showAlert(Alert.Plain, 
            "This is japeserver, the platform-independent GUI for the Jape proof engine"+
                (version==null ? "" : ", working with "+version)+
                ". Jape and japeserver copyright © 2002 Richard Bornat & Bernard Sufrin.");
        /* when I get round to it, give access to this text:
        Jape and japeserver are free software; you can redistribute them and/or
modify them under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

Jape and japeserver are distributed in the hope that they will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with jape; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA,
or go to http://www.gnu.org.

*/
    }
	
    public static void setVersion(String _version) {
        version = _version;
    }
	
}