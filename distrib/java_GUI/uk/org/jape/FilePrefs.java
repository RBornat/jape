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

package uk.org.jape;
import java.io.File;

import java.util.prefs.Preferences;

public class FilePrefs {
    
    private static final String lastOpenDirKey = "LastOpenedDirectory",
				lastSavedDirKey = "LastSavedDirectory";
    
    public static void setLastOpenedDir(File path) { 
		Preferences prefs = Preferences.userNodeForPackage(FilePrefs.class);
		prefs.put(lastOpenDirKey, path.toString());
    }
    
    public static void setLastSavedDir(File path) { 
		Preferences prefs = Preferences.userNodeForPackage(FilePrefs.class);
		prefs.put(lastSavedDirKey, path.toString());
    }
    
    public static File nextOpen() {
		return nextOpen(true);
    }
    
    private static File nextOpen(boolean firsttime) {
		Preferences prefs = Preferences.userNodeForPackage(FilePrefs.class);
		String lastdir = prefs.get(lastOpenDirKey, null);
		if (lastdir==null) {
			if (firsttime) {
			setLastOpenedDir(new File(System.getProperties().getProperty("user.dir")));
			return nextOpen(false);
			}
			else {
			Logger.log.println("nextOpen failed");
			return new File(System.getProperties().getProperty("user.dir"));
			}
		}
		else
			return new File(lastdir);
    }
    
    public static File nextSave() {
		Preferences prefs = Preferences.userNodeForPackage(FilePrefs.class);
		String lastdir = prefs.get(lastSavedDirKey, null);
		return lastdir==null ? nextOpen() : new File(lastdir);
    }
    
}
