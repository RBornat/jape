//
//  Menu.java
//  japeserver
//
//  Created by Richard Bornat on Fri Aug 30 2002.
//  Copyright (c) 2002 __MyCompanyName__. All rights reserved.
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
    
    private void indexMenu(JMenuBar bar, String label) {
        if (menutable.get(label)==null) {
            JMenu menu = new JMenu(label);
            if (japeserver.onMacOS) {
                JapeFont.setComponentFont(menu.getComponent());
            }
            menutable.put(label,menu);
            bar.add(menu);
        }
    }
    
    // may throw NullPointerException ...
    private JMenuItem indexMenuItem(String menulabel, JapeMenuItem action) {
        JMenu menu = (JMenu)menutable.get(menulabel);
        String label = action.getText();
        String key = menulabel + ": " + label;
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
    
    public void addStdFileMenuItems() {
        indexMenuItem("File", new OpenFileAction("Open...")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_O, menumask));
		
        indexMenuItem("File", new CmdAction("Open new theory...", "reset;reload"));

        indexMenuItem("File", new DummyAction("Close", "File: Close")).
             setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W, menumask));
		
        indexMenuItem("File", new DummyAction("Save", "File: Save")).
             setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, menumask));
		
        indexMenuItem("File", new DummyAction("Save As...", "File: Save As..."));
    }
	
	
    public void addStdEditMenuItems() {
        indexMenuItem("Edit", new DummyAction("Undo", "Edit: Undo")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z, menumask));
        
        ((JMenu)menutable.get("Edit")).addSeparator();

        indexMenuItem("Edit", new DummyAction("Cut", "Edit: Cut")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_X, menumask));

        indexMenuItem("Edit", new DummyAction("Copy", "Edit: Copy")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_C, menumask));

        indexMenuItem("Edit", new DummyAction("Paste", "Edit: Paste")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_V, menumask));

        indexMenuItem("Edit", new DummyAction("Clear", "Edit: Clear"));

        indexMenuItem("Edit", new DummyAction("Select All", "Edit: Select All")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A, menumask));
    }
	
    private JFrame frame=null;
    
    public void addStdMenus(JFrame frame) {
	JMenuBar bar = new JMenuBar();        
        indexMenu(bar, "File"); indexMenu(bar, "Edit");
        
        addStdFileMenuItems();
        addStdEditMenuItems();
        
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
            indexMenuItem(menuname, new CmdAction(label, cmd)); 
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
