/* 
    $Id$

    Copyright � 2002 Richard Bornat & Bernard Sufrin
     
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

// the MacOSX version of LocalSettings.  All it does is catch the menu actions from
// the application menu and divert them to japeserver static methods.

import java.awt.Dimension;
import java.lang.IllegalStateException;

import com.apple.mrj.*;

public class LocalSettings implements  MRJAboutHandler,
                                       MRJQuitHandler,
                                       MRJPrefsHandler {

    // parameters
                                           
    public static final boolean panelWindowMenus = true;
    
    public static final boolean aboutMenuItemNeeded = false;
    public static final boolean quitMenuItemNeeded = false;
    public static final boolean prefsMenuItemNeeded = false;

    public static final Dimension proofPanelDefaultSize = new Dimension(200,200);

    // MacOS specific bits
    public void handleAbout() {
        japeserver.handleAbout();
    }

    public void handleQuit() throws IllegalStateException {
        japeserver.handleQuit();
        throw new IllegalStateException(); // if we return from handleQuit, we didn't exit
    }
    
    public void handlePrefs() {
        japeserver.handlePrefs();
    }
    
    public LocalSettings() {
        MRJApplicationUtils.registerAboutHandler(this);
        MRJApplicationUtils.registerQuitHandler(this);
        MRJApplicationUtils.registerPrefsHandler(this);
    }
}
