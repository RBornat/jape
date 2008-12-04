/* 
    $Id$

    Copyright © 2003-8 Richard Bornat & Bernard Sufrin

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

public class FilePrefs {
    
    private static final String lastOpenDirKey  = "LastOpenedDirectory",
				lastSavedDirKey = "LastSavedDirectory",
				recentFilesKey  = "RecentFiles";
    
    public static void setLastOpenedDir(File path) { 
	JapePrefs.prefs.put(lastOpenDirKey, path.toString());
    }
    
    public static void setLastSavedDir(File path) { 
	JapePrefs.prefs.put(lastSavedDirKey, path.toString());
    }
    
    public static File nextOpen() {
	return nextOpen(true);
    }
    
    private static File nextOpen(boolean firsttime) {
	String lastdir = JapePrefs.prefs.get(lastOpenDirKey, null);
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
	String lastdir = JapePrefs.prefs.get(lastSavedDirKey, null);
	return lastdir==null ? nextOpen() : new File(lastdir);
    }
    
    public static int recentMax = 15; /* should itself be a preference */
    
    public static void recordRecentFile(String fstring) {
	if (Jape.onUnix) {
	    String home = System.getProperties().getProperty("user.home");
	    if (fstring.startsWith("~"))
		fstring = home+fstring.substring(1);
	}
	File file = new File(fstring);
	try {
	    fstring = file.getCanonicalPath();
	} catch (java.io.IOException e) { }
	if (Jape.onUnix) {
	    String home = System.getProperties().getProperty("user.home");
	    if (fstring.startsWith(home))
		fstring = "~"+fstring.substring(home.length());
	}
	String recent = JapePrefs.prefs.get(recentFilesKey, "");
	String r = fstring+"\n";
	int i = 0, j, count=1;
	while ((j=recent.indexOf("\n", i))!=-1) {
	    String next = recent.substring(i, j);
	    if (count<recentMax && !(next.equals(fstring))){
		r = r+next+"\n";
		count++;
	    }
	    i = j+1;
	}
	JapeMenu.setRecentFiles(r, true);
	JapePrefs.prefs.put(recentFilesKey, r);
    }
    
    public static String getRecentFiles() {
	String recent = JapePrefs.prefs.get(recentFilesKey, "");
	return recent;
    }
    
    public static void clearRecentFiles() {
	JapePrefs.prefs.put(recentFilesKey, "");
	JapeMenu.setRecentFiles("", true);
    }
}
