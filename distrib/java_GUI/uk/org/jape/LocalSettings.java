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

// this class, which can be replaced by a platform-specific version to aid 
// look-and-feel localisation (see for example jape/MacOSX/java_japeserver/LocalSettings.java)
// or I don't know what else.

import java.awt.Dimension;

public class LocalSettings  {
	
	// these overridden in MacOSX LocalSettings
        
        public static final boolean panelWindowMenus = false;
        
	public static final boolean aboutMenuItemNeeded = true;
	public static final boolean quitMenuItemNeeded = true;
	public static final boolean prefsMenuItemNeeded = true;

        public static final Dimension proofPanelDefaultSize = new Dimension(200,200);
}
