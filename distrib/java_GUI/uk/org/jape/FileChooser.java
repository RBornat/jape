//
//  FileChooser.java
//  japeserver
//
//  Created by Richard Bornat on Wed Sep 04 2002.
//  Copyleft 2002 Richard Bornat & Bernard Sufrin. Proper GPL text to be inserted
//

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
}
