// 
// $Id$
//
//  Copyleft 2002 Richard Bornat & Bernard Sufrin. Proper GPL text to be inserted
//

// the MacOSX version of LocalSettings.  All it does is catch the menu actions from
// the application menu and divert them to japeserver static methods.

import com.apple.mrj.*;

public class LocalSettings implements  MRJAboutHandler,
                                       MRJQuitHandler,
                                       MRJPrefsHandler {
    
    public static final boolean panelWindowMenus = true;
    
    public static final boolean aboutMenuItemNeeded = false;
    public static final boolean quitMenuItemNeeded = false;
    public static final boolean prefsMenuItemNeeded = false;
    
    public void handleAbout() {
        japeserver.handleAbout();
    }

    public void handleQuit() {
        japeserver.handleQuit();
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
