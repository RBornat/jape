/* 
    $Id$

    Copyright � 2002 Richard Bornat & Bernard Sufrin
     
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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.ButtonGroup;
import java.util.Enumeration;
import java.awt.Font;
import java.awt.Graphics;
import java.util.Hashtable;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.KeyStroke;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.util.Vector;

public class JapeMenu implements DebugConstants {
    
    /*	Java doesn't want to share menu bars or menus or menu items, so far
        as I can tell.  So I have to make a menu factory and build a new
        menu bar for each window that needs one.
     */
    
    protected static Vector barv = new Vector(); // of Ms
    
    protected static class M {
        final boolean proofsonly; final String title;
        protected final Vector itemv=new Vector(); // of Is and Seps and RBGs and CBs
        M(boolean proofsonly, String title) { this.proofsonly=proofsonly; this.title=title; }
        private static final I quitItem = new I("Quit", "", null),
                               doneItem = new I("Done", "", null);
        private int nextIndex() {
            int size = itemv.size(), i;
            if (title.equals("File") && (i=itemv.indexOf(quitItem))!=-1)
                return i-1; // point to separator
            else
            if (title.equals("Edit") && (i=itemv.indexOf(doneItem))!=-1)
                return i-1; // point to separator
            else
                return  size;
        }
        public void add(I i) { itemv.insertElementAt(i,nextIndex()); }
        public void add(RBG rbg) { itemv.insertElementAt(rbg,nextIndex()); }
        public void addSep() {
            int i = nextIndex();
            if (preSep(i))
                itemv.insertElementAt(new Sep(),i);
        }
        public I findI(String label) throws ProtocolError {
            int vc = itemv.size();
            for (int i=0; i<vc; i++) {
                Object o = itemv.get(i);
                if (o instanceof I && ((I)o).label.equals(label))
                    return (I)o;
                if (o instanceof RBG) {
                    RBG g = (RBG)o;
                    for (int j=0; j<g.items.length; j++)
                        if (g.items[j].label.equals(label))
                            return g.items[j];
                }
            }
            throw new ProtocolError("no item \""+label+"\" in menu "+title);
        }
        public boolean preSep(int i) {
            return 0<=i-1 && i-1<itemv.size() && !(itemv.get(i-1) instanceof Sep);
        }
        public boolean postSep(int i) {
            return 0<=i+1 && i+1<itemv.size() &&
                   !(itemv.get(i+1) instanceof Sep || itemv.get(i+1) instanceof RBG);
        }
        public int size() { return itemv.size(); }
        public Object get(int i) { return itemv.get(i); }
        public String toString() {
            String s = "M[\""+title+"\" [";
            for (int i=0; i<itemv.size(); i++) {
                s = s+itemv.get(i);
                if (i+1<itemv.size())
                    s = s+", ";
            }
            return s+"]]";
        }
    }
    
    protected static class I {
        String label;
        String key;
        ItemAction action;
        KeyStroke stroke;
        boolean enabled, selected;
        I(String label, String key, ItemAction action) {
            this.label=label; this.key=key; this.action=action; this.stroke=null;
            this.enabled=true; this.selected=false;
        }
        public void setAccelerator(KeyStroke stroke) { this.stroke=stroke; }
        public void setEnabled(boolean enabled) { this.enabled = enabled; }
        public void setSelected(boolean selected) { this.selected = selected; }
        public String toString() { return "I"+contentsToString(); }
        public String contentsToString() {
            return  "[label=\""+label+"\""+
                    " key=\""+key+"\""+
                    " action="+action+
                    " stroke="+stroke+
                    " enabled="+enabled+
                    " selected="+selected+
                    "]";
        }
        // label equality only
        public boolean equals(Object o) {
            return o instanceof I && ((I)o).label.equals(label);
        }
    }
    
    protected static class Sep {
        public String toString() { return "Sep"; }
    }

    protected static class RBG {
        I[] items;
        RBG(I[] items) { this.items=items; }
        public String toString() {
            String s = "RBG[";
            for (int i=0; i<items.length; i++) {
                s = s+items[i];
                if (i+1<items.length)
                    s = s+", ";
            }
            return s+"]";
        }
    }

    protected static class CB extends I {
        boolean ready;
        CB(String label, String key, ItemAction action) {
            super(label, key, action);
            // experimentally, it seems that we get ActionPerformed twice for checkboxes ...
            ready = true;
        }
        public String toString() {
            return "CB"+contentsToString();
        }
    }

    private static void mkItem(JMenu menu, I i, JMenuItem item) {
        item.setActionCommand(i.key);
        if (i.stroke!=null)
            item.setAccelerator(i.stroke);
        item.setEnabled(i.enabled);
        item.setSelected(i.selected);
        JapeFont.setComponentFont(item.getComponent(), JapeFont.MENUENTRY);
        item.addActionListener(menuListener);
        if (menuaction_tracing)
            System.err.println("ActionListener on "+i);
        menu.add(item);
    }

    protected static class TitledMenuBar extends JMenuBar {
        public TitledMenuBar() { super(); }
        public TitledMenu getMenu(String s) {
            int mc = getMenuCount();
            for (int i=0; i<mc; i++) {
                TitledMenu m = (TitledMenu)getMenu(i);
                if (m.getText().equals(s))
                    return m;
            }
            return null;
        }
    }

    protected static class TitledMenu extends JMenu {
        public TitledMenu(String title) { super(title); }
        public JMenuItem getItem(String s) {
            int ic = getItemCount();
            for (int i=0; i<ic; i++) {
                JMenuItem ji = getItem(i); // could be a separator ...
                if (ji!=null && ji.getText().equals(s))
                    return ji;
            }
            return null;
        }
    }
    
    protected static TitledMenuBar mkBar(boolean isProofBar) {
        TitledMenuBar bar = new TitledMenuBar();
        for (Enumeration ebar = barv.elements(); ebar.hasMoreElements(); ) {
            M m = (M)ebar.nextElement();
            if (!m.proofsonly || isProofBar) {
                TitledMenu menu = new TitledMenu(m.title);
                JapeFont.setComponentFont(menu.getComponent(), JapeFont.MENUENTRY);
                for (int i=0; i<m.size(); i++) {
                    Object o = m.get(i);
                    if (o instanceof RBG) {
                        if (m.preSep(i))
                            menu.addSeparator();
                        RBG rbg = (RBG)o;
                        ButtonGroup group = new ButtonGroup();
                        for (int j=0; j<rbg.items.length; j++) {
                            JRadioButtonMenuItem item = new JRadioButtonMenuItem(rbg.items[j].label, j==0);
                            mkItem(menu, rbg.items[j], item);
                            group.add(item);
                        }
                        if (m.postSep(i))
                            menu.addSeparator();
                    }
                    else
                    if (o instanceof CB) {
                        CB cb = (CB)o;
                        JCheckBoxMenuItem item = new JCheckBoxMenuItem(cb.label);
                        mkItem(menu, cb, item);
                    }
                    else
                    if (o instanceof I) {
                        I ii = (I)o;
                        if (isProofBar || !m.title.equals("File") || !ii.label.equals("Close")) {
                            JMenuItem item = new JMenuItem(ii.label);
                            mkItem(menu, ii, item);
                        }
                    }
                    else
                    if (o instanceof Sep)
                        menu.addSeparator();
                    else
                        Alert.abort("JapeMenu.mkBar sees "+o);
                }
                bar.add(menu);
            }
        }
        return bar;
    }

    private static void setBar(boolean isProofBar, JapeWindow w) {
        w.setJMenuBar(mkBar(isProofBar));
        w.getJMenuBar().revalidate();
    }

    public static void setProofWindowBar(JapeWindow w) { setBar(true, w); }

    public static void setNonProofWindowBar(JapeWindow w) { setBar(false, w); }
    
    public static void makeMenusVisible() {
    	JapeWindow.updateMenuBars(); 
    }
    
    // I need dictionaries from menu titles to Ms and item keys to Is: 
    // hashtables are overkill, but there you go
    
    private static Hashtable menutable, actiontable;

    private static M ensureMenu(String menuname) throws ProtocolError {
        M menu = (M)menutable.get(menuname);
        if (menu==null)
            throw new ProtocolError("no menu named "+menuname);
        return menu;
    }
    
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
    
    private static class OpenFileAction extends ItemAction {
        public void action () {
             String file = FileChooser.newOpenDialog("theories, logic files and proofs", "jt", "j", "jp");
             if (file.length()!=0)
                 Reply.sendCOMMAND("use \""+file+"\"");
        }
    
    }

    private static class CloseProofAction extends ItemAction {
        public void action() {
            ProofWindow.closeFocussedProof();
        }
    }
    
    private static M indexMenu(boolean proofsonly, String label) {
        M menu = (M)menutable.get(label);
        if (menu==null) {
            menu = new M(proofsonly, label);
            menutable.put(label,menu);
            barv.add(menu);
        }
        return menu;
    }
    
    private static String keyString(String menu, String label) {
        return menu + ": " + label;
    }

    private static I makeI(String menuname, String label, ItemAction action) {
        String key = keyString(menuname, label);
        I i = new I(label, key, action);
        actiontable.put(key,i);
        return i;
    }
    
    private static I indexMenuItem(M menu, String label, ItemAction action) {
        I i = makeI(menu.title, label, action);
        menu.add(i);
        return i;
    }
    
    private static int menumask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
    
    public static void addStdFileMenuItems(M filemenu) {
        if (LocalSettings.aboutMenuItemNeeded) {
            class AboutBoxAction extends ItemAction {
                public void action() { japeserver.handleAbout(); }
            }
            indexMenuItem(filemenu, "About jape", new AboutBoxAction());
            filemenu.addSep();
        }
        
        indexMenuItem(filemenu, "Open...", new OpenFileAction()).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_O, menumask));
		
        indexMenuItem(filemenu, "Open new theory...", new CmdAction("reset;reload"));

        indexMenuItem(filemenu, "Close", new CloseProofAction()).
             setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W, menumask));
		
        indexMenuItem(filemenu, "Save Proofs", new CmdAction("saveproofs false")).
             setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, menumask));
		
        indexMenuItem(filemenu, "Save Proofs As...", new CmdAction("saveproofs true"));

        filemenu.addSep();

        indexMenuItem(filemenu, "Erase theory", new CmdAction("reset"));
        
        if (LocalSettings.quitMenuItemNeeded) {
            filemenu.addSep();
            class QuitAction extends ItemAction {
                public void action() { japeserver.handleQuit(); }
            }
            indexMenuItem(filemenu, "Quit", new QuitAction());
        }
    }
	
    public static void addStdEditMenuItems(M editmenu) {
        indexMenuItem(editmenu, "Undo", new UnimplementedAction("Edit: Undo")).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z, menumask));
        
        editmenu.addSep();

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
            editmenu.addSep();
            class PrefsAction extends ItemAction {
                public void action() { japeserver.handlePrefs(); }
            }
            indexMenuItem(editmenu, "Preferences...", new PrefsAction());
        }
    }
	
    public static void newMenuBar() {
	barv = new Vector();        
        M filemenu = indexMenu(false, "File"); 
        M editmenu = indexMenu(true, "Edit");
        
        addStdFileMenuItems(filemenu);
        addStdEditMenuItems(editmenu);
    }
    
    public static void newMenu(boolean proofsonly, String s) throws ProtocolError {
        indexMenu(proofsonly, s);
    }
    
    // ActionListener interface (for menus)
    public static boolean CheckboxDoubleBounce = false; // calamity: see japserver.main
    
    protected static class MenuListener implements ActionListener {
        public void actionPerformed(ActionEvent newEvent) {
            if (menuaction_tracing)
                System.err.println("JapeMenu.actionPerformed "+newEvent);
            String key = newEvent.getActionCommand();
            I i = (I)actiontable.get(key);
            if (i!=null) {
                if (CheckboxDoubleBounce && i instanceof CB) {
                    // experimentally, it seems that checkboxes can get two actionPerformed events ...
                    CB cb = (CB)i;
                    if (menuaction_tracing)
                        System.err.println("CB "+cb.ready);
                    cb.ready = !cb.ready;
                    if (!cb.ready)
                        return;
                }
                i.action.action();
            }
            else
                Alert.showErrorAlert("unrecognised menu action "+key);
        }
    }

    protected static final MenuListener menuListener = new MenuListener();
    
    public static void init() {
        // this is the reset action, too
        menutable = new Hashtable(20,(float)0.5);
        actiontable = new Hashtable(100,(float)0.5);
        newMenuBar();
    }

    public static void cancelMenus() {
        init(); // I hope
    }

    public static void emptyMenus() {
        init(); // I hope
    }

    public static void addSeparator(String menuname) throws ProtocolError {
        try {
            M menu = (M)menutable.get(menuname);
            menu.addSep();
        } catch (Exception e) {
            throw new ProtocolError("failed");
        }
    }

    public static void addItem(String menuname, String label, String code, String cmd)
        throws ProtocolError {
        M menu = ensureMenu(menuname);
        I item = (I)actiontable.get(keyString(menuname, label));
        ItemAction action = new CmdAction(cmd);
        if (item==null)
            indexMenuItem(menu, label, action);
            // and what do we do about code?
        else
            item.action = action;
    }

    private static void doEnableItem(JapeWindow w, String menuname, String label, boolean enable) {
        if (w!=null) {
            TitledMenuBar bar = (TitledMenuBar)w.getJMenuBar();
            if (bar!=null) {
                TitledMenu tm = bar.getMenu(menuname);
                if (tm!=null) {
                    JMenuItem jmi = tm.getItem(label);
                    if (jmi!=null)
                        jmi.setEnabled(enable);
                    else
                    if (true || menuaction_tracing)
                        System.err.println("no item "+label+" in menu "+menuname+" in window "+w);
                }
                else
                if (menuaction_tracing)
                    System.err.println("no menu "+menuname+" in window "+w);
            }
        }
    }
    
    public static void enableItem(boolean focussedonly, String menuname, String label, boolean enable)
        throws ProtocolError {
        try {
            M menu = ensureMenu(menuname);
            I action =menu.findI(label);

            if (!focussedonly)
                action.setEnabled(enable);

            if (focussedonly)
                doEnableItem(ProofWindow.focussedProofWindow(false), menuname, label, enable);
            else
            for (Enumeration e = JapeWindow.windows(); e.hasMoreElements(); )
                doEnableItem((JapeWindow)e.nextElement(), menuname, label, enable);
        } catch (ProtocolError e) {
            if (menuname.equals("Edit") && label.equals("Disprove"))
                return;
            else
                throw e;
        }
    }

    public static void addRadioButtonGroup(String menuname, String[][] rbs) throws ProtocolError {
        M menu = ensureMenu(menuname);
        I[] rbg = new I[rbs.length];
        for (int i=0; i<rbs.length; i++)
            rbg[i] = makeI(menuname, rbs[i][0], new CmdAction(rbs[i][1]));
        menu.add(new RBG(rbg));
    }

    public static void addCheckBox(String menuname, String label, String cmd)
        throws ProtocolError {
        M menu = ensureMenu(menuname);
        String key = keyString(menuname, label);
        CB cb = new CB(label, key, new CmdAction(cmd));
        actiontable.put(key,cb);
        menu.add(cb);
    }

    private static void doTickItem(JapeWindow w, String menuname, String label, boolean state) {
        if (w!=null) {
            TitledMenuBar bar = (TitledMenuBar)w.getJMenuBar();
            if (bar!=null) {
                TitledMenu tm = bar.getMenu(menuname);
                if (tm!=null) {
                    JMenuItem jmi = tm.getItem(label);
                    if (jmi!=null)
                        jmi.setSelected(state);
                    else
                    if (true || menuaction_tracing)
                        System.err.println("no item "+label+" in menu "+menuname+" in window "+w);
                }
                else
                if (menuaction_tracing)
                    System.err.println("no menu "+menuname+" in window "+w);
            }
        }
    }
    
    public static void tickItem(boolean focussedonly, String menuname, String label, boolean state)
        throws ProtocolError {
        M menu = ensureMenu(menuname);
        I item = menu.findI(label);

        item.setSelected(state);

        if (focussedonly)
            doTickItem(ProofWindow.focussedProofWindow(false), menuname, label, state);
        else
            for (Enumeration e = JapeWindow.windows(); e.hasMoreElements(); )
                doTickItem((JapeWindow)e.nextElement(), menuname, label, state);
    }
}
