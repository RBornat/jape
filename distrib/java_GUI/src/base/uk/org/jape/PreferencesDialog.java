package uk.org.jape;

import java.awt.Frame;
import java.awt.event.KeyEvent;
import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

public class PreferencesDialog {
    
    @SuppressWarnings("serial")
    public static class PreferencesPane extends JPanel {
        public final JTabbedPane tabbedPane;
        public final JapeFont.FontPanel fontpanel;
        public final DebugVars.DebugPanel debugpanel;
        PreferencesPane() {
            tabbedPane = new JTabbedPane();
            ImageIcon icon =  null; // createImageIcon("images/middle.gif");

            fontpanel = new JapeFont.FontPanel();
            tabbedPane.addTab("Fonts", icon, fontpanel);
            tabbedPane.setMnemonicAt(0, KeyEvent.VK_1);

            debugpanel = new DebugVars.DebugPanel();
            tabbedPane.addTab("Debug logging", icon, debugpanel);
            tabbedPane.setMnemonicAt(1, KeyEvent.VK_2);
            
            this.add(tabbedPane);
        }
    }
    
    public static void handlePrefs() {
        //Create and set up the window.
        Frame parent = JapeWindow.getTopWindow();
         
        PreferencesPane pane = new PreferencesPane();
        
        int q = Alert.askOKCancel(parent, pane);
        if (q==Alert.OK) {
            JapeFont.actUpon(pane.fontpanel);
            DebugVars.actUpon(pane.debugpanel);
        }
 
        /*dialog.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent we) {
                Alert.showAlert("we got close?");
                dialog.setVisible(false);
            }
        });

        //Display the window.
        dialog.pack();
        dialog.setVisible(true);*/
    }

    /** Returns an ImageIcon, or null if the path was invalid. */
/*  protected static ImageIcon createImageIcon(String path) {
        java.net.URL imgURL = TabbedPaneDemo.class.getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL);
        } else {
            System.err.println("Couldn't find file: " + path);
            return null;
        }
    } */
    
}
