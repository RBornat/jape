// 
// $Id$
//
//  Copyleft 2002 Richard Bornat & Bernard Sufrin. Proper GPL text to be inserted
//

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Enumeration;
import java.awt.Font;
import java.awt.Graphics;
import java.util.Hashtable;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.util.Vector;

public class JapeMenu implements ActionListener {
    
    /* Java doesn't want to share menu bars or menus or menu items, so far
       as I can tell.  So I have to make a menu factory and build a new
       menu bar for each window that needs one.
     */
    
    protected Vector barv = new Vector(); // of Ms
    
    protected static class M {
        final String title;
        final Vector itemv; // of Is and Seps
        M(String title) { this.title=title; itemv=new Vector(); }
        public void add(I i) { itemv.add(i); }
        public void addSeparator() { itemv.add(new Sep()); }
    }
    
    protected static class I {
        String label;
        String key;
        ItemAction action;
        KeyStroke stroke;
        boolean enabled;
        I(String label, String key, ItemAction action) {
            this.label=label; this.key=key; this.action=action; this.stroke=null; this.enabled=true;
        }
        public void setAccelerator(KeyStroke stroke) { this.stroke=stroke; }
        public void setEnabled(boolean enabled) { this.enabled = enabled; }
    }
    
    protected static class Sep {
    }
    
    protected JMenuBar mkBar() {
        JMenuBar bar = new JMenuBar();
        for (Enumeration ebar = barv.elements(); ebar.hasMoreElements(); ) {
            M m = (M)ebar.nextElement();
            JMenu menu = new JMenu(m.title);
            JapeFont.setComponentFont(menu.getComponent());
            for (Enumeration emenu = m.itemv.elements(); emenu.hasMoreElements(); ) {
                Object o = emenu.nextElement();
                if (o instanceof Sep) 
                    menu.addSeparator();
                else {
                    I i = (I)o;
                    JMenuItem item = new JMenuItem(i.label);
                    item.setActionCommand(i.key);
                    if (i.stroke!=null)
                        item.setAccelerator(i.stroke);
                    if (!i.enabled)
                        item.setEnabled(i.enabled);
                    JapeFont.setComponentFont(item.getComponent());
                    item.addActionListener(this);
                    menu.add(item);
                }
            }
            bar.add(menu);
        }
        return bar;
    }
    
    public void setBar(JapeWindow w) {
        w.setJMenuBar(mkBar());
    }
    
    public static void makeMenusVisible() {
    	JapeWindow.updateMenuBars(); 
    }
    
    // I need dictionaries from menu titles to Ms and item keys to Is: 
    // hashtables are overkill, but there you go
    
    private Hashtable menutable, actiontable;
    
    private abstract static class ItemAction {
        abstract public void action();
    }
    
    private static class UnimplementedAction extends ItemAction {
        String s;
        UnimplementedAction (String s) { this.s = s; }
        public void action () {  System.err.println(s); }
    }
    
    private static class CmdAction extends ItemAction {
        String cmd;
        CmdAction (String cmd) { this.cmd = cmd; }
        public void action () {
             Reply.sendCOMMAND(cmd);
        }
    }
    
    private class OpenFileAction extends ItemAction {
        public void action () {
             String file = FileChooser.newOpenDialog("theories, logic files and proofs", "jt", "j", "jp");
             if (file.length()!=0)
                Reply.sendCOMMAND("use "+file);
        }
    
    }
    
    private M indexMenu(String label) {
        M menu = (M)menutable.get(label);
        if (menu==null) {
            menu = new M(label);
            menutable.put(label,menu);
            barv.add(menu);
        }
        return menu;
    }
    
    private String keyString(String menu, String label) {
        return menu + ": " + label;
    }
    
    // may throw NullPointerException ...
    private I indexMenuItem(M menu, String label, ItemAction action) {
        String key = keyString(menu.title, label);
        I i = new I(label, key, action);
        actiontable.put(key,i);
        menu.add(i);
        return i;
    }
    
    private int menumask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
    
    public void addStdFileMenuItems(M filemenu) {
        if (LocalSettings.aboutMenuItemNeeded) {
            class AboutBoxAction extends ItemAction {
                public void action() { japeserver.handleAbout(); }
            }
            indexMenuItem(filemenu, "About jape", new AboutBoxAction());
            filemenu.addSeparator();
        }
        
        indexMenuItem(filemenu, "Open...", new OpenFileAction()).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_O, menumask));
		
        indexMenuItem(filemenu, "Open new theory...", new CmdAction("reset;reload"));

        indexMenuItem(filemenu, "Close", new UnimplementedAction("File: Close")).
             setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W, menumask));
		
        indexMenuItem(filemenu, "Save", new UnimplementedAction("File: Save")).
             setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, menumask));
		
        indexMenuItem(filemenu, "Save As...", new UnimplementedAction("File: Save As..."));
        
        if (LocalSettings.quitMenuItemNeeded) {
            filemenu.addSeparator();
            class QuitAction extends ItemAction {
                public void action() { japeserver.handleQuit(); }
            }
            indexMenuItem(filemenu, "Quit", new QuitAction());
        }
    }
	
    public void addStdEditMenuItems(M editmenu) {
        indexMenuItem(editmenu, "Undo", new UnimplementedAction("Edit: Undo")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z, menumask));
        
        editmenu.addSeparator();

        indexMenuItem(editmenu, "Cut", new UnimplementedAction("Edit: Cut")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_X, menumask));

        indexMenuItem(editmenu, "Copy", new UnimplementedAction("Edit: Copy")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_C, menumask));

        indexMenuItem(editmenu, "Paste", new UnimplementedAction("Edit: Paste")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_V, menumask));

        indexMenuItem(editmenu, "Clear", new UnimplementedAction("Edit: Clear"));

        indexMenuItem(editmenu, "Select All", new UnimplementedAction("Edit: Select All")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A, menumask));
        
        if (LocalSettings.prefsMenuItemNeeded) {
            editmenu.addSeparator();
            class PrefsAction extends ItemAction {
                public void action() { japeserver.handlePrefs(); }
            }
            indexMenuItem(editmenu, "Preferences...", new PrefsAction());
        }
    }
	
    public void newMenuBar() {
	barv = new Vector();        
        M filemenu = indexMenu("File"); 
        M editmenu = indexMenu("Edit");
        
        addStdFileMenuItems(filemenu);
        addStdEditMenuItems(editmenu);
    }
    
    public void newMenu(String s) throws ProtocolError {
        indexMenu(s);
    }
    
    // ActionListener interface (for menus)
    public void actionPerformed(ActionEvent newEvent) {
        String key = newEvent.getActionCommand();
        I i = (I)actiontable.get(key);
        if (i!=null)
            i.action.action();
        else 
            Alert.showErrorAlert("unrecognised menu action "+key);
    }

    public JapeMenu() {
        // this is the reset action, too
        menutable = new Hashtable(20,(float)0.5);
        actiontable = new Hashtable(100,(float)0.5);
        newMenuBar();
    }

    public void menusep(String menuname) throws ProtocolError {
        try {
            M menu = (M)menutable.get(menuname);
            menu.addSeparator();
        } catch (Exception e) {
            throw new ProtocolError("failed");
        }
    }

    public void newMenuItem(String menuname, String label, String code, String cmd) throws ProtocolError {
        try {
            indexMenuItem((M)menutable.get(menuname), label, new CmdAction(cmd)); 
            // and what do we do about code?
        } catch (Exception e) {
            throw new ProtocolError("failed");
        }
    }

    public void enablemenuitem(String menuname, String label, boolean enable) {
        int mi, ii;
        try {
            M menu = (M)menutable.get(menuname);
            mi = barv.indexOf(menu);
            I action = (I)actiontable.get(keyString(menuname, label)); 
            ii = menu.itemv.indexOf(action);
            action.setEnabled(enable);
        } catch (Exception e) {
            System.err.println("ENABLEMENUITEM \""+menuname+"\" \""+label+"\" "+enable+" failed");
            return;
        }
        
        for (Enumeration e = JapeWindow.windows(); e.hasMoreElements(); ) {
            JapeWindow w = (JapeWindow)e.nextElement();
            JMenuBar bar = w.getJMenuBar();
            if (bar!=null)
                bar.getMenu(mi).getItem(ii).setEnabled(enable);
        }
    }
}
