// 
// $Id$
//
//  Copyleft 2002 Richard Bornat & Bernard Sufrin. Proper GPL text to be inserted
//

import java.util.Enumeration;
import java.awt.Font;
import java.util.Iterator;
import javax.swing.JFrame;
import javax.swing.JMenuBar;
import java.util.Vector;

public class JapeWindow extends JFrame {

    String title;
    protected static Vector windowv = new Vector();
    
    public JapeWindow(String title) {
        super(title);
        this.title=title; // ignoring whatever else may happen outside, this is a uid
        windowv.add(this);
    }
    
    public static JapeWindow findWindow(String title) {
        int len = windowv.size();
        for (int i=0; i<len; i++) {
            JapeWindow w = (JapeWindow)windowv.get(i);
            if (w.title.equals(title))
                return w;
        }
        return null;
    }
    
    public static Enumeration windows() {
        return windowv.elements();
    }
    
    private static void setBar(JapeWindow w) {
        if (w instanceof ProofWindow || 
            (w instanceof PanelWindow && LocalSettings.panelWindowMenus) ||
            w instanceof SurrogateWindow) // the surrogate needs all the menus, till we implement PROOFMENU 
            // even panel windows get a menu on MacOS X
            japeserver.menus.setBar(w);
    }
    
    public static void updateMenuBars() {
        for (Enumeration e = windowv.elements(); e.hasMoreElements(); ) {
            JapeWindow w = (JapeWindow)e.nextElement();
            setBar(w);
        }
    }
    
}
