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

/* Apparently FileDialog looks better than JFileChooser in MacOS X Aqua, and may look better in Windows. */

import java.awt.FileDialog;
import java.io.File;

import javax.swing.JFileChooser;

public class FileChooser /* implements FilenameFilter */ {

    public static String newOpenDialog(String message) {
	return newOpenDialog(message, new String[]{});
    }
    
    public static String newOpenDialog(String message, String extension) {
	return newOpenDialog(message, new String[]{extension});
    }
    
    public static String newOpenDialog(String message, String [] extension) {
	JapeFileFilter filter = new JapeFileFilter(message);
	for (int i=0; i<extension.length; i++)
	    filter.addExtension(extension[i]);
	
	String result = "";
	
	if (Jape.onMacOSX) { // use AWT
	    FileDialog d = new FileDialog(JapeWindow.getTopWindow(), message, FileDialog.LOAD);
	    d.setDirectory(FilePrefs.nextOpen().toString());
	    d.setFilenameFilter(filter);
	    d.setVisible(true);
	    String file = d.getFile();
	    String dir = d.getDirectory();
	    d.dispose();
	    if (file!=null && dir!=null) {
		File fdir = new File(dir);
		FilePrefs.setLastOpenedDir(fdir);
		result = (new File(dir,file)).toString();
	    }
	    else
		return "";
	} 
	else { // use Swing
	    JFileChooser chooser = new JFileChooser(FilePrefs.nextOpen());
	    chooser.setFileFilter(filter);
	    int returnVal = chooser.showOpenDialog(null);
	    File selected = chooser.getSelectedFile();
	    if (returnVal==JFileChooser.APPROVE_OPTION) {
		File dir = selected.getParentFile();
		if (dir!=null) FilePrefs.setLastOpenedDir(dir);
		result = selected.toString();
	    } 
	    else
		return "";
	}
	
	FilePrefs.recordRecentFile(result);
	return result;
    }

    /* extension shouldn't be empty, because it confuses FilePrefs */
    public static String newSaveDialog(String message, String suggestion, String extension) {
	JapeFileFilter filter = new JapeFileFilter(message);
	filter.addExtension(extension);
	
	File saveFile = FilePrefs.getLastSave(extension);
	File saveDir = saveFile.isDirectory() ? saveFile : saveFile.getParentFile();
	String saveName = saveFile.isDirectory() ? (suggestion == null ? "Untitled" : suggestion)+"."+extension :
	                  suggestion == null ? saveFile.getName() : suggestion+"."+extension;
	
	if (Jape.onMacOSX) { // use AWT
	    FileDialog d = new FileDialog(JapeWindow.getTopWindow(), message, FileDialog.SAVE);
            d.setDirectory(saveDir.getPath());
            d.setFile(saveName);
	    d.setFilenameFilter(filter);
	    d.setVisible(true);
	    String file = d.getFile();
	    String dir = d.getDirectory();
	    d.dispose();
	    saveFile = file!=null && dir!=null ? new File(dir,file) : null;
	} 
	else { // use Swing
	    JFileChooser chooser = new JFileChooser(saveDir);
	    chooser.setSelectedFile(new File(saveName));
	    chooser.setFileFilter(filter);
	    
	    int returnVal = chooser.showSaveDialog(null);
	    File selected  =  chooser.getSelectedFile();
	    if (returnVal==JFileChooser.APPROVE_OPTION)
		saveFile = selected;
	    else
                saveFile = null;
	}
	
        if (saveFile!=null) {
            FilePrefs.setLastSave(saveFile.getPath(), extension);
            return saveFile.getPath(); 
        }
        else
            return "";
    }
}


