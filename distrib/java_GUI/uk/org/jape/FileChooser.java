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

public class FileChooser {
    private static String doOpenDialog(JFileChooser chooser) {
        int returnVal = chooser.showOpenDialog(null);
        return (returnVal==JFileChooser.APPROVE_OPTION ? chooser.getSelectedFile().toString() : "");
    }
    
    // oh tedium .. why can't I have a list argument?
    public static String newOpenDialog(String message) {
        return doOpenDialog(new JFileChooser());
    }

    public static String newOpenDialog(String message, String extension) {
        JFileChooser chooser = new JFileChooser();
        ExampleFileFilter filter = new ExampleFileFilter();
        filter.addExtension(extension);
        filter.setDescription(message);
        chooser.setFileFilter(filter);
        return doOpenDialog(chooser);
    }

    public static String newOpenDialog(String message, String ext1, String ext2) {
        JFileChooser chooser = new JFileChooser();
        ExampleFileFilter filter = new ExampleFileFilter();
        filter.addExtension(ext1);
        filter.addExtension(ext2);
        filter.setDescription(message);
        chooser.setFileFilter(filter);
        return doOpenDialog(chooser);
    }
    public static String newOpenDialog(String message, String ext1, String ext2, String ext3) {
        JFileChooser chooser = new JFileChooser();
        ExampleFileFilter filter = new ExampleFileFilter();
        filter.addExtension(ext1);
        filter.addExtension(ext2);
        filter.addExtension(ext3);
        filter.setDescription(message);
        chooser.setFileFilter(filter);
        return doOpenDialog(chooser);
    }

    private static String doSaveDialog(JFileChooser chooser) {
        int returnVal = chooser.showSaveDialog(null);
        return (returnVal==JFileChooser.APPROVE_OPTION ? chooser.getSelectedFile().toString() : "");
    }

    public static String newSaveDialog(String message) {
        return doSaveDialog(new JFileChooser());
    }

    public static String newSaveDialog(String message, String extension) {
        JFileChooser chooser = new JFileChooser();
        ExampleFileFilter filter = new ExampleFileFilter();
        filter.addExtension(extension);
        filter.setDescription(message);
        chooser.setFileFilter(filter);
        return doSaveDialog(chooser);
    }

    public static String newSaveDialog(String message, String ext1, String ext2) {
        JFileChooser chooser = new JFileChooser();
        ExampleFileFilter filter = new ExampleFileFilter();
        filter.addExtension(ext1);
        filter.addExtension(ext2);
        filter.setDescription(message);
        chooser.setFileFilter(filter);
        return doSaveDialog(chooser);
    }
    public static String newSaveDialog(String message, String ext1, String ext2, String ext3) {
        JFileChooser chooser = new JFileChooser();
        ExampleFileFilter filter = new ExampleFileFilter();
        filter.addExtension(ext1);
        filter.addExtension(ext2);
        filter.addExtension(ext3);
        filter.setDescription(message);
        chooser.setFileFilter(filter);
        return doSaveDialog(chooser);
    }
}
