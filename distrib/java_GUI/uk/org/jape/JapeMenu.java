//
//  Menu.java
//  japeserver
//
//  Created by Richard Bornat on Fri Aug 30 2002.
//  Copyright (c) 2002 __MyCompanyName__. All rights reserved.
//

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.*;

public class JapeMenu implements  ActionListener {
    
    // I need dictionaries from menu names to menus and menu entries to actions: 
    // hashtables are overkill, but there you go
    private Hashtable menutable, actiontable;
    
    // Declarations for menus
    private static final JMenuBar mainMenuBar = new JMenuBar();

    private abstract class JapeMenuItem extends JMenuItem {
        JapeMenuItem(String label) { super(label); } // but don't use me, of course (daft language)
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
        OpenFileAction (String label) {
            super(label);
        }
        public void action () {
             String file = FileChooser.newOpenDialog("theories, logic files and proofs", "jt", "j", "jp");
             if (file.length()!=0)
                Reply.sendCOMMAND("use "+file);
        }
    
    }
    
    private void indexMenu(String label) {
        JMenu menu = new JMenu(label);
        menutable.put(label,menu);
    }
    
    private JMenuItem indexMenuItem(String menulabel, JapeMenuItem action) {
        JMenu menu = (JMenu)menutable.get(menulabel); // needs an exception
        String label = action.getText();
        String key = menulabel + ": " + label;
        actiontable.put(key,action);
        action.setActionCommand(key);
        menu.add(action); // .setEnabled(true)
        action.addActionListener(this);
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
		
        mainMenuBar.add((JMenu)menutable.get("File"));
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

        mainMenuBar.add((JMenu)menutable.get("Edit"));
    }
	
    public void addMenus(JFrame frame) {
        indexMenu("File"); indexMenu("Edit");
        
        addStdFileMenuItems();
        addStdEditMenuItems();
        frame.setJMenuBar (mainMenuBar);
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

    public void menuentry(String menuname, String label, String code, String cmd) {
        try {
            JMenu menu = (JMenu)menutable.get(menuname);
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
