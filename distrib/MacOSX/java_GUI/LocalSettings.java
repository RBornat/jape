//
//  LocalSettings.java
//  japeserver
//
//  Created by Richard Bornat on Fri Sep 13 2002.
//  Copyleft 2002 Richard Bornat & Bernard Sufrin. Proper GPL text to be inserted
//

// the MacOSX version of LocalSettings.  All it does is catch the menu actions from
// the application menu and divert them to japeserver static methods.

import com.apple.mrj.*;

public class LocalSettings implements  MRJAboutHandler,
                                       MRJQuitHandler,
                                       MRJPrefsHandler {
    
    public static final boolean aboutMenuItemNeeded = false;
    public static final boolean quitMenuItemNeeded = false;
    public static final boolean prefsMenuItemNeeded = false;
    
    public void handleAbout() {
        // System.err.println("in LocalSettings.handleAbout");
        japeserver.handleAbout();
    }

    public void handleQuit() {
        // System.err.println("in LocalSettings.handleQuit");
        japeserver.handleQuit();
    }
    
    public void handlePrefs() {
        // System.err.println("in LocalSettings.handlePrefs");
        japeserver.handlePrefs();
    }
    
    public LocalSettings() {
        MRJApplicationUtils.registerAboutHandler(this);
        MRJApplicationUtils.registerQuitHandler(this);
        MRJApplicationUtils.registerPrefsHandler(this);
    }
}
