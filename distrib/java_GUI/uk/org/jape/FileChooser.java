/* 
    $Id$

    Copyright © 2003 Richard Bornat & Bernard Sufrin
     
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

/* Apparently FileDialog looks better than JFileChooser in Aqua, and may look better in Windows. */

import java.awt.FileDialog;

import java.io.File;
import java.io.FilenameFilter;

import javax.swing.JFileChooser;

public class FileChooser /* implements FilenameFilter */ {

    private static File lastOpen=null, lastSave=null; 

    public static void setOpen(File path) { lastOpen = path; }

    private static File nextOpen() {
        return (lastOpen!=null ? lastOpen :
                lastSave!=null ? lastSave :
                                 new File(System.getProperties().getProperty("user.dir")));
    }

    private static File nextSave() {
        return (lastSave!=null ? lastSave :
                lastOpen!=null ? lastOpen :
                                 new File(System.getProperties().getProperty("user.dir")));
    }

    private static String doOpenDialog(JFileChooser chooser) {
        int returnVal = chooser.showOpenDialog(null);
        File selected = chooser.getSelectedFile();
        if (returnVal==JFileChooser.APPROVE_OPTION) {
            File dir = selected.getParentFile();
            if (dir!=null) lastOpen = dir;
            return selected.toString();
        } 
        else
            return "";
    }
    
    private static String doSaveDialog(JFileChooser chooser) {
        int returnVal = chooser.showSaveDialog(null);
        File selected  =  chooser.getSelectedFile();
        if (returnVal==JFileChooser.APPROVE_OPTION) {
            File dir = selected.getParentFile();
            if (dir!=null) lastSave = dir;
            return selected.toString();
        } 
        else
            return "";
    }

    public static String newOpenDialog(String message) {
        return newOpenDialog(message, new String[]{});
    }

    public static String newOpenDialog(String message, String [] extension) {
        /* if (Jape.onMacOS && lastOpen==null) {
            Logger.log.println("\".\" translates to "+(new File(".").getAbsolutePath())+"\n"+
                                  "and nextOpen().getAbsolutePath()="+nextOpen().getAbsolutePath());
        } */
        JFileChooser chooser = new JFileChooser(nextOpen());
        ExampleFileFilter filter = new ExampleFileFilter();
        for (int i=0; i<extension.length; i++)
            filter.addExtension(extension[i]);
        filter.setDescription(message);
        chooser.setFileFilter(filter);
        return doOpenDialog(chooser);
    }

    public static String newSaveDialog(String message, String [] extension) {
        JFileChooser chooser = new JFileChooser(nextSave());
        ExampleFileFilter filter = new ExampleFileFilter();
        for (int i=0; i<extension.length; i++)
            filter.addExtension(extension[i]);
        filter.setDescription(message);
        chooser.setFileFilter(filter);
        return doSaveDialog(chooser);
    }

    public static String newOpenDialog(String message, String extension) {
        return newOpenDialog(message, new String[]{extension});
    }

    public static String newOpenDialog(String message, String ext1, String ext2) {
        return newOpenDialog(message, new String[]{ext1, ext2});
    }
    
    public static String newOpenDialog(String message, String ext1, String ext2, String ext3) {
        return newOpenDialog(message, new String[]{ext1, ext2, ext3});
    }

    public static String newSaveDialog(String message) {
        return newSaveDialog(message, new String[]{});
    }

    public static String newSaveDialog(String message, String extension) {
        return newSaveDialog(message, new String[]{extension});
    }

    public static String newSaveDialog(String message, String ext1, String ext2) {
        return newSaveDialog(message, new String[]{ext1, ext2});
    }
    
    public static String newSaveDialog(String message, String ext1, String ext2, String ext3) {
        return newSaveDialog(message, new String[]{ext1, ext2, ext3});
    }

}


