//
//  Menu.java
//  japeserver
//
//  Created by Richard Bornat on Fri Aug 30 2002.
//  Copyleft 2002 Richard Bornat & Bernard Sufrin. Proper GPL text to be inserted
//

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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

public class JapeMenu implements ActionListener {
    
    // I need dictionaries from menu names to menus and menu entries to actions: 
    // hashtables are overkill, but there you go
    private Hashtable menutable, actiontable;
    
    private abstract class JapeMenuItem extends JMenuItem {
        protected JapeMenuItem(String label) { super(label); }
        public abstract void action();
    }
    
    private class DummyAction extends JapeMenuItem {
        String s;
        DummyAction (String label, String s) {
            super(label);
            this.s = s;
        }
        public void action () {
           System.err.println(s);
        }
    }
    
    private class CmdAction extends JapeMenuItem {
        String cmd;
        CmdAction (String label, String cmd) {
            super(label);
            this.cmd = cmd;
        }
        public void action () {
             Reply.sendCOMMAND(cmd);
        }
    }
    
    private class OpenFileAction extends JapeMenuItem {
        public OpenFileAction (String label) {
            super(label);
        }
        public void action () {
             String file = FileChooser.newOpenDialog("theories, logic files and proofs", "jt", "j", "jp");
             if (file.length()!=0)
                Reply.sendCOMMAND("use "+file);
        }
    
    }
    
    private JMenu indexMenu(JMenuBar bar, String label) {
        JMenu menu = (JMenu)menutable.get(label);
        if (menu==null) {
            menu = new JMenu(label);
            if (japeserver.onMacOS) {
                JapeFont.setComponentFont(menu.getComponent());
            }
            menutable.put(label,menu);
            bar.add(menu);
        }
        return menu;
    }
    
    // may throw NullPointerException ...
    private JMenuItem indexMenuItem(JMenu menu, JapeMenuItem action) {
        String label = action.getText();
        String key = menu.getText() + ": " + label;
        actiontable.put(key,action);
        action.setActionCommand(key);
        menu.add(action); // .setEnabled(true)
        action.addActionListener(this);
        if (japeserver.onMacOS) {
            JapeFont.setComponentFont(action.getComponent());
        }
        return action;
    }
    
    private int menumask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
    
    public void addStdFileMenuItems(JMenu filemenu) {
        if (LocalSettings.aboutMenuItemNeeded) {
            class AboutBoxAction extends JapeMenuItem {
                AboutBoxAction(String label) { super(label); }
                public void action() { japeserver.handleAbout(); }
            }
            indexMenuItem(filemenu, new AboutBoxAction("About jape"));
            filemenu.addSeparator();
        }
        
        indexMenuItem(filemenu, new OpenFileAction("Open...")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_O, menumask));
		
        indexMenuItem(filemenu, new CmdAction("Open new theory...", "reset;reload"));

        indexMenuItem(filemenu, new DummyAction("Close", "File: Close")).
             setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W, menumask));
		
        indexMenuItem(filemenu, new DummyAction("Save", "File: Save")).
             setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, menumask));
		
        indexMenuItem(filemenu, new DummyAction("Save As...", "File: Save As..."));
        
        if (LocalSettings.quitMenuItemNeeded) {
            filemenu.addSeparator();
            class QuitAction extends JapeMenuItem {
                QuitAction(String label) { super(label); }
                public void action() { japeserver.handleQuit(); }
            }
            indexMenuItem(filemenu, new QuitAction("Quit"));
        }
    }
	
    public void addStdEditMenuItems(JMenu editmenu) {
        indexMenuItem(editmenu, new DummyAction("Undo", "Edit: Undo")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z, menumask));
        
        editmenu.addSeparator();

        indexMenuItem(editmenu, new DummyAction("Cut", "Edit: Cut")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_X, menumask));

        indexMenuItem(editmenu, new DummyAction("Copy", "Edit: Copy")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_C, menumask));

        indexMenuItem(editmenu, new DummyAction("Paste", "Edit: Paste")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_V, menumask));

        indexMenuItem(editmenu, new DummyAction("Clear", "Edit: Clear"));

        indexMenuItem(editmenu, new DummyAction("Select All", "Edit: Select All")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A, menumask));
        
        if (LocalSettings.prefsMenuItemNeeded) {
            editmenu.addSeparator();
            class PrefsAction extends JapeMenuItem {
                PrefsAction(String label) { super(label); }
                public void action() { japeserver.handlePrefs(); }
            }
            indexMenuItem(editmenu, new PrefsAction("Preferences..."));
        }
    }
	
    private JFrame frame=null;
    
    public void addStdMenus(JFrame frame) {
	JMenuBar bar = new JMenuBar();        
        JMenu filemenu = indexMenu(bar, "File"); 
        JMenu editmenu = indexMenu(bar, "Edit");
        
        addStdFileMenuItems(filemenu);
        addStdEditMenuItems(editmenu);
        
        this.frame = frame;
        frame.setJMenuBar(bar);
    }
    
    public void newMenu(String s) throws ProtocolError {
        if (frame==null)
            throw (new ProtocolError("newMenu with null frame??"));
        else {
            JMenuBar bar = frame.getJMenuBar();
            indexMenu(bar, s);
            frame.setJMenuBar(bar);
        }
    }
    
    // ActionListener interface (for menus)
    public void actionPerformed(ActionEvent newEvent) {
        String key = newEvent.getActionCommand();
        JapeMenuItem action = (JapeMenuItem)actiontable.get(key);
        if (action!=null)
            action.action();
        else 
            System.err.println("unrecognised menu action "+key);
    }

    public JapeMenu() {
        menutable = new Hashtable(20,(float)0.5);
        actiontable = new Hashtable(100,(float)0.5);
    }

    public void menusep(String menuname) {
        try {
            JMenu menu = (JMenu)menutable.get(menuname);
            menu.addSeparator();
        } catch (Exception e) {
            System.err.println("MENUSEP \""+menuname+"\" failed");
        }
    }

    public void newMenuItem(String menuname, String label, String code, String cmd) {
        try {
            indexMenuItem((JMenu)menutable.get(menuname), new CmdAction(label, cmd)); 
            // and what do we do about code?
        } catch (Exception e) {
            System.err.println("MENUENTRY \""+menuname+"\" \""+label+"\" \""+code+"\" \""+cmd+"\" failed");
        }
    }

    public void enablemenuitem(String menuname, String label, boolean enable) {
        try {
            JMenu menu = (JMenu)menutable.get(menuname);
            JapeMenuItem action = (JapeMenuItem)actiontable.get(menuname+": "+label); 
            action.setEnabled(enable);
        } catch (Exception e) {
            System.err.println("ENABLEMENUITEM \""+menuname+"\" \""+label+"\" "+enable+" failed");
        }
    }
}
