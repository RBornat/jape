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

import javax.swing.*;
import java.io.File;

public class FileChooser {

    private static String 
                lastOpen = ".",   // these are paths
                lastSave = ".";
    

    private static String doOpenDialog(JFileChooser chooser) {
        int   returnVal = chooser.showOpenDialog(null);
        File  selected  =  chooser.getSelectedFile();
        if (returnVal==JFileChooser.APPROVE_OPTION)
        { String dir = selected.getParent();
          if (dir!=null) lastOpen = dir;
          return selected.toString();
        } 
        else
        {
          return "";
        }
    }
    
    private static String doSaveDialog(JFileChooser chooser) {
        int   returnVal = chooser.showSaveDialog(null);
        File  selected  =  chooser.getSelectedFile();
        if (returnVal==JFileChooser.APPROVE_OPTION)
        { String dir = selected.getParent();
          if (dir!=null) lastSave = dir;
          return selected.toString();
        } 
        else
        {
          return "";
        }
    }

    // oh tedium .. why can't I have a list argument?
    public static String newOpenDialog(String message) {
        return newOpenDialog(message, new String[]{});
    }

    // oh rtfm! You can  [HE'S BACK!]
    public static String newOpenDialog(String message, String [] extension) {
        JFileChooser chooser = new JFileChooser(lastOpen);
        ExampleFileFilter filter = new ExampleFileFilter();
        for (int i=0; i<extension.length; i++)
            filter.addExtension(extension[i]);
        filter.setDescription(message);
        chooser.setFileFilter(filter);
        return doOpenDialog(chooser);
    }

    public static String newSaveDialog(String message, String [] extension) {
        JFileChooser chooser = new JFileChooser(lastSave);
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

