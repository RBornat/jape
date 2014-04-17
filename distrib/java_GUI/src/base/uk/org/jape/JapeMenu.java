/* 
    $Id$

    Copyright © 2003-8 Richard Bornat & Bernard Sufrin
     
        richard@bornat.me.uk
        sufrin@comlab.ox.ac.uk

    This file is part of the Jape GUI, which is part of Jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).
    
*/

package uk.org.jape;

import java.awt.Toolkit;
import java.awt.Window;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.KeyStroke;
import javax.swing.UIManager;

public class JapeMenu implements DebugConstants {
    
    /*  Java can't share menu bars or menus or menu items, because of the
        containment hierarchy idea.

        So this is a menu bar / menu / menu item factory.

        It seems to get very confused when allocating menu bars quite often
        (which happens because of the Window menu).  So I'm using version stamps
        to try to cut down the traffic ...
     */

    private static int versionstamp = 0;
    
    private static Vector<M> barv = new Vector<M>(); // of Ms
    
    protected static class M {
        final int barKinds; final String title;
        protected final Vector<MO> itemv=new Vector<MO>(); // of Is and Seps and RBGs and CBs
        M(String title, int barKinds) { 
            this.barKinds=barKinds; this.title=title; 
        }
        private static final I quitItem = new I("Quit", "", null, ALL_BARS),
                               doneItem = new I("Done", "", null, PROOFWINDOW_BAR);
        public void clear() {
            itemv.removeAllElements();
        }
        public int nextIndex() {
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
        public void insert(int index, I i) { itemv.insertElementAt(i,index); }
        public void add(RBG rbg) { itemv.insertElementAt(rbg,nextIndex()); }
        public void add(RFM rfm) { itemv.insertElementAt(rfm,nextIndex()); }
        public void addSep() { addSep(this.barKinds); }
        public void addSep(int barKinds) { insertSep(nextIndex(), barKinds); }
        public void insertSep(int i) { insertSep(i, this.barKinds); }
        public void insertSep(int i, int barKinds) {
            if (preSep(i))
                itemv.insertElementAt(new Sep(barKinds),i);
        }
        public void removeI(String label) {
            // Logger.log.println("JapeMenu.M.removeI "+JapeUtils.enQuote(label)+" from "+this);
            int vc = itemv.size();
            for (int i=0; i<vc; i++) {
                Object o = itemv.get(i);
                if (o instanceof I && ((I)o).label.equals(label)) {
                    itemv.remove(i); 
                    // Logger.log.println("menu now "+this);
                    versionstamp++; // seems to be necessary
                    return;
                }
            }
            Alert.abort("JapeMenu.M.removeI "+JapeUtils.enQuote(label)+" from "+this);
        }
        public void removeSep(int index) {
            if (index<itemv.size() && itemv.get(index) instanceof Sep)
                itemv.remove(index);
            // else doesn't matter: only happens in Window menu
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
            throw new ProtocolError("no item "+JapeUtils.enQuote(label)+" in menu "+title);
        }
        public boolean preSep(int i) {
            return 0<i && i<=itemv.size() && 
                    !(itemv.get(i-1) instanceof Sep); // why no RBG? confusion here ...
        }
        public boolean postSep(int i) {
            return 0<=i+1 && i+1<itemv.size() &&
                   !(itemv.get(i+1) instanceof Sep || itemv.get(i+1) instanceof RBG);
        }
        public int size() { return itemv.size(); }
        public Object get(int i) { return itemv.get(i); }
        public String toString() {
            String s = "M["+JapeUtils.enQuote(title)+" [";
            for (int i=0; i<itemv.size(); i++) {
                s = s+itemv.get(i);
                if (i+1<itemv.size())
                    s = s+", ";
            }
            return s+"]]";
        }
    }
    
    protected static class MO {
        final int barKinds;
        MO(int barKinds) {
            this.barKinds = barKinds;
        }
    }
    
    protected static class I extends MO {
        final String label;
        final String key;
        ItemAction action;
        KeyStroke stroke;
        boolean enabled, selected;
        I(String label, String key, ItemAction action, int barKinds) {
            super(barKinds);
            this.label=label; this.key=key; this.action=action; this.stroke=null;
            this.enabled=true; this.selected=false;
        }
        public void setAccelerator(KeyStroke stroke) { this.stroke=stroke; }
        public void setEnabled(boolean enabled) { this.enabled = enabled; }
        public void setSelected(boolean selected) { this.selected = selected; }
        public String toString() { return "I"+contentsToString(); }
        public String contentsToString() {
            return  "[label="+JapeUtils.enQuote(label)+
                    " key="+JapeUtils.enQuote(key)+
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
    
    protected static class Sep extends MO {
        Sep(int barKinds) {
            super(barKinds);
        }
        public String toString() { return "Sep"; }
    }

    protected static class RBG extends MO {
        I[] items;
        RBG(I[] items, int barKinds) { 
            super(barKinds);
            this.items=items; 
        }
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
        CB(String label, String key, ItemAction action, int barKinds) {
            super(label, key, action, barKinds);
            // experimentally, it seems that we get ActionPerformed twice for checkboxes ...
            ready = true;
        }
        public String toString() {
            return "CB"+contentsToString();
        }
    }
    
    /* just for Open Recent */
    protected static class RFM extends MO {
        RFM(int barKinds) { super(barKinds); }
    }

    private static void mkItem(JMenu menu, I i, JMenuItem item, ActionListener listener) {
        item.setActionCommand(i.key);
        if (i.stroke!=null)
            item.setAccelerator(i.stroke);
        item.setEnabled(i.enabled);
        item.setSelected(i.selected);
        JapeFont.setComponentFont(item.getComponent(), JapeFont.MENUENTRY);
        if (DebugVars.menuaction_tracing)
            Logger.log.println("mkItem "+JapeUtils.enQuote(menu.getText())+
                            " "+JapeUtils.enQuote(item.getText())+
                            " addActionListener("+listener+")");
        item.addActionListener(listener);
        if (DebugVars. menuaction_tracing)
            Logger.log.println("ActionListener on "+i);
        menu.add(item);
    }

    @SuppressWarnings("serial")
    protected static class TitledMenuBar extends JMenuBar {
        public TitledMenuBar() { super(); }
        public TitledMenu getMenu(String title) {
            int mc = getMenuCount();
            for (int i=0; i<mc; i++) {
                TitledMenu m = (TitledMenu)getMenu(i);
                if (m.getText().equals(title))
                    return m;
            }
            return null;
        }
    }

    @SuppressWarnings("serial")
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

    private static final String RADIO_ICON_KEY = "RadioButtonMenuItem.checkIcon",
                                CHECK_ICON_KEY = "CheckBoxMenuItem.checkIcon";

    public static final String COPY            = "Copy",
                               EXPORT          = "Export",
                               EXPORT_DISPROOF = "Export Disproof",
                               EXPORT_PROOF    = "Export Proof",
                               MAKE_LEMMA      = "Make lemma...",
                               OPEN_RECENT     = "Open Recent";
    
    public static final int PROOFWINDOW_BAR      = 1,
                            DIALOGWINDOW_BAR     = 2,
                            TEXTDIALOGWINDOW_BAR = 4,
                            OTHERWINDOW_BAR      = 8;
    
    public static final int ALL_BARS = PROOFWINDOW_BAR | DIALOGWINDOW_BAR |
                                       TEXTDIALOGWINDOW_BAR | OTHERWINDOW_BAR,
                            EDIT_BARS = PROOFWINDOW_BAR | TEXTDIALOGWINDOW_BAR,
                            UNDIALOG_BARS = ALL_BARS - (DIALOGWINDOW_BAR | TEXTDIALOGWINDOW_BAR);
    
    protected static TitledMenuBar mkBar(int barKind, Window w, String title) {
        if (DebugVars.menuaction_tracing)
            Logger.log.println("mkBar("+barKind+","+JapeUtils.enQuote(title)+")");
        Object radioIcon = UIManager.get(RADIO_ICON_KEY);
        Object checkIcon = UIManager.get(CHECK_ICON_KEY);
        TitledMenuBar bar = new TitledMenuBar();
        ActionListener listener = new MenuItemListener(w);
        for (Enumeration<M> ebar = barv.elements(); ebar.hasMoreElements(); ) {
            M m = (M)ebar.nextElement();
            if ((m.barKinds & barKind)!=0) {
                TitledMenu menu = new TitledMenu(m.title);
                boolean isWindowMenu = (LocalSettings.windowMenuItemsTicked && 
                                        m.title.equals("Window"));
                ButtonGroup buttonGroup = isWindowMenu ? new ButtonGroup() : null;
                if (isWindowMenu && radioIcon!=null && checkIcon!=null) {
                    // make RadioButtons which look like CheckBoxes
                    UIManager.put(RADIO_ICON_KEY, checkIcon);
                }
                JapeFont.setComponentFont(menu.getComponent(), JapeFont.MENUENTRY);
                for (int i=0; i<m.size(); i++) {
                    MO mo = (MO)m.get(i);
                    if ((mo.barKinds&barKind)!=0) {
                        if (mo instanceof RBG) {
                            if (m.preSep(i))
                                menu.addSeparator();
                            RBG rbg = (RBG)mo;
                            ButtonGroup group = new ButtonGroup();
                            for (int j=0; j<rbg.items.length; j++) {
                                JRadioButtonMenuItem item = new JRadioButtonMenuItem(rbg.items[j].label, j==0);
                                mkItem(menu, rbg.items[j], item, listener);
                                group.add(item);
                            }
                            if (m.postSep(i))
                                menu.addSeparator();
                        }
                        else
                        if (mo instanceof CB) {
                            CB cb = (CB)mo;
                            JCheckBoxMenuItem item = new JCheckBoxMenuItem(cb.label);
                            mkItem(menu, cb, item, listener);
                        }
                        else
                        if (mo instanceof RFM)
                            menu.add(recentFilesMenu());
                        else
                        if (mo instanceof I) {
                            I ii = (I)mo;
                            if (buttonGroup==null) {
                                JMenuItem item = new JMenuItem(ii.label);
                                mkItem(menu, ii, item, listener);
                            }
                            else {
                                JRadioButtonMenuItem item =
                                new JRadioButtonMenuItem(ii.label, i==0);
                                if (DebugVars.menuaction_tracing)
                                    Logger.log.println("window menu item "+ii.label+
                                                       "; window="+title);
                                mkItem(menu, ii, item, listener);
                                buttonGroup.add(item);
                            }
                        }
                        else
                        if (mo instanceof Sep)
                            menu.addSeparator();
                        else
                            Alert.abort("JapeMenu.mkBar sees "+mo);
                    }
                }
                if (isWindowMenu && radioIcon!=null && checkIcon!=null)
                    UIManager.put(RADIO_ICON_KEY, radioIcon);
                bar.add(menu);
            }
        }
        return bar;
    }

    private static void setJMenuBar(int barKind, JapeWindow w) {
        if (DebugVars.menuaction_tracing)
            Logger.log.println("JapeMenu.setJMenuBar "+barKind+" "+JapeUtils.enQuote(w.title));
        w.setJMenuBar(mkBar(barKind, w, w.title));
        w.getJMenuBar().revalidate();
        if (DebugVars.menuaction_tracing)
            Logger.log.println(JapeUtils.enQuote(w.title)+" now has menu bar "+w.getJMenuBar());
    }

    public static int setBar(JapeWindow w, int stamp) {
        if (DebugVars.menuaction_tracing)
            Logger.log.println("JapeMenu.setBar "+JapeUtils.enQuote(w.title)+
                               " "+stamp+
                               " ("+versionstamp+"; "+(w.getJMenuBar()!=null)+")");
        if (w.getJMenuBar()==null || stamp<versionstamp) {
            if (w instanceof ProofWindow)
                setJMenuBar(PROOFWINDOW_BAR, w);
            else
            if ((w instanceof PanelWindowData.PanelWindow && LocalSettings.panelWindowMenus) ||
                w instanceof SurrogateWindow || w instanceof Logger.LogWindow)
                setJMenuBar(OTHERWINDOW_BAR, w);
        }
        return versionstamp; 
    }
    
    // for some mysterious reason, this doesn't work in front of the surrogate window
    
    public static void setDialogMenuBar(int barKinds, JDialog w, String title) {
        if (Jape.onMacOSX) {
            setJMenuBar(barKinds, JapeWindow.getTopWindow());
        }
        else {
            w.setJMenuBar(mkBar(barKinds, w, title));
            w.getJMenuBar().revalidate();
        }
    }
    
    public static void undoDialogMenuBar() {
        if (Jape.onMacOSX) {
            setBar(JapeWindow.getTopWindow(), -1);
        }
    }
    
    // I need dictionaries from menu titles to Ms and item keys to Is: 
    // hashtables are overkill, but there you go -- so much of Java is.
    
    private static Hashtable<String, M> menutable;
    private static Hashtable<String, I> itemtable;

    private static M ensureMenu(String menuname) throws ProtocolError {
        M menu = (M)menutable.get(menuname);
        if (menu==null)
            throw new ProtocolError("no menu named "+menuname);
        return menu;
    }

    /* ******************************* menu actions ******************************* */

    private abstract static class ItemAction {
        abstract public void action(Window w);
    }

    private static class AboutBoxAction extends ItemAction {
        public void action(Window w) { Jape.handleAbout(); }
    }

    private static class ActivateWindowAction extends ItemAction {
        Window w;
        public ActivateWindowAction(Window w) {
            super(); this.w=w;
        }
        public void action(Window ignore) {
            w.toFront();
        }
    }

    private static class AlertAction extends ItemAction {
        String s;
        AlertAction (String s) { this.s = s; }
        public void action (Window w) {  Alert.showAlert(Alert.Warning, s); }
    }

    private static class CloseProofAction extends ItemAction {
        public void action(Window w) {
            if (w instanceof ProofWindow)
                ((ProofWindow)w).closeProof();
            else
                Alert.abort("CloseProofAction on non-proof window");
        }
    }

    private static class CmdAction extends ItemAction {
        String cmd;
        CmdAction (String cmd) { this.cmd = cmd; }
        public void action (Window w) {
             Reply.sendCOMMAND(cmd);
        }
    }

    private static class CopyProofAction extends ItemAction {
        public void action (Window w) {
            Alert.showAlert(Alert.Info,
                            "Copy Proof doesn't do anything yet. Export (see File menu) can make a "+
                            (Jape.onMacOSX ? "pdf or ps" : "PostScript (ps)")+" file");
        }
    }

    private static class CopyUnicodeAction extends ItemAction {
        public void action (Window w) {
            if (w instanceof ProofWindow) {
                String s = ((ProofWindow)w).getSingleTextSelection();
                if (s!=null)
                    Toolkit.getDefaultToolkit().getSystemClipboard().
                        setContents(new StringSelection(s),null);
            }
            else
                Alert.abort("CopyUnicodeAction on non-proof window");
        }
    }

    /* private static class CopyAsciiAction extends ItemAction {
        public void action (Window w) {
            if (w instanceof ProofWindow) {
                String s = ((ProofWindow)w).getSingleTextSelection();
                if (s!=null) {
                    try {
                        byte bs[] = JapeCharEncoding.tranString(s);
                        // oh hackery, thy name is Bornat!
                        String s1 = new String(bs); // this'll fool em ...
                        Toolkit.getDefaultToolkit().getSystemClipboard().
                            setContents(new StringSelection(s1),null);
                    } catch (java.io.IOException e) {
                        Alert.showAlert(Alert.Error, "Can't translate "+JapeUtils.enQuote(s)+
                                        " into ASCII");
                    }
                }
            }
            else
                Alert.abort("CopyAsciiAction on non-proof window");
        }
    } */ 

    private static class DebugSettingsAction extends ItemAction {
        public void action (Window w) {
            DebugVars.runDebugSettingsDialog();
        }
    }
    
    private static class ExportAction extends ItemAction {
        public void action(Window w) {
            if (w instanceof ProofWindow)
                Export.export((ProofWindow)w, PrintProof.BOTH);
            else
                Alert.abort("ExportAction on non-proof window");
        }
    }
    
    private static class ExportDisproofAction extends ItemAction {
        public void action(Window w) {
            if (w instanceof ProofWindow)
                Export.export((ProofWindow)w, PrintProof.DISPROOF);
            else
                Alert.abort("ExportDisproofAction on non-proof window");
        }
    }
    
    private static class ExportProofAction extends ItemAction {
        public void action(Window w) {
            if (w instanceof ProofWindow)
                Export.export((ProofWindow)w, PrintProof.PROOF);
            else
                Alert.abort("ExportProofAction on non-proof window");
        }
    }
    
    private static class FontSizesAction extends ItemAction {
        public void action (Window w) {
            JapeFont.runFontSizesDialog();
        }
    }

    private static class OpenFileAction extends ItemAction {
        public void action (Window w) {
            doOpenFile(chooseFile());
        }
    }
    
    private static class MakeLemmaAction extends ItemAction {
        public void action (Window w) {
            Reply.sendCOMMAND("makelemma");
        }
    }
    
    public static String chooseFile() {
        return FileChooser.newOpenDialog("theories, logic files and proofs", 
                                         new String[]{"jt", "j", "jp"});
    }

    public static String chooseTheory() {
       return FileChooser.newOpenDialog("theories", "jt");
    }

    public static void doOpenFile(String file) {
        if (file.length()!=0)
            Reply.sendCOMMAND("use", file);

    }

    private static class PrefsAction extends ItemAction {
        public void action(Window w) { Jape.handlePrefs(); }
    }

    private static class PageSetupAction extends ItemAction {
        public void action(Window w) { PrintProof.pageSetup(); }
    }

    private static class PrintProofAction extends ItemAction {
        public void action(Window w) {
            if (w instanceof ProofWindow)
                PrintProof.printProof((ProofWindow)w);
            else
                Alert.abort("PrintProofAction on non-proof window");
        }
    }
    
    private static class RedoAction extends ItemAction {
        public void action(Window w) {
            if (w instanceof ProofWindow)
                Reply.sendCOMMAND("redo_"+((ProofWindow)w).undoSuffix());
            else
                Alert.abort("RedoAction not in ProofWindow");
        }
    }
    
    private static class ShowWindowAction extends ItemAction {
        JFrame w;
        public ShowWindowAction(JFrame w) {
            super(); this.w=w;
        }
        public void action(Window ignore) {
            w.setVisible(true);
            w.toFront();
        }
    }
    
    private static class TextCommandAction extends ItemAction {
        public void action(Window w) { TextDialog.runTextCommandDialog(); }
    }
    
    private static class TacticCommandAction extends ItemAction {
        public void action(Window w) { 
            if (w instanceof ProofWindow)
                TextDialog.runTacticCommandDialog();
            else
                Alert.abort("You can't apply a tactic from a non-proof window");
        }
    }
    
    
    private static class UndoAction extends ItemAction {
        public void action(Window w) {
            if (w instanceof ProofWindow)
                Reply.sendCOMMAND("undo_"+((ProofWindow)w).undoSuffix());
            else
                Alert.abort("UndoAction not in ProofWindow");
        }
    }
    
    private static class QuitAction extends ItemAction {
        public void action(Window w) { Jape.handleQuit(); }
    }

    private static class UnimplementedAction extends ItemAction {
        String s;
        UnimplementedAction (String s) { super(); this.s = s; }
        public void action (Window w) {  Logger.log.println(s); }
    }

    private static boolean insertbefore(M menu, String label) {
        M other = (M)menutable.get(label);
        if (other!=null) {
            barv.add(barv.indexOf(other), menu); return true;
        }
        else
            return false;
    }

    private static boolean append(M menu) {
        barv.add(menu); return true;
    }

    private static M indexMenu(M menu, String label) {
        versionstamp++;
        menutable.put(label,menu);
        @SuppressWarnings("unused")
        boolean dummy;
        if (label.equals("Help"))
            dummy = append(menu); // Help at the end
        else
        if (label.equals("Window"))
            dummy = insertbefore(menu,"Help") || append(menu); // Window before Help
        else
            dummy = insertbefore(menu,"Window") || insertbefore(menu,"Help") || append(menu); // before Window
        return menu;
    }        

    private static M indexMenu(String label, int barKinds) {
        M menu = (M)menutable.get(label);
        if (menu==null) {
            return indexMenu(new M(label, barKinds), label);
        }
        return menu;
    }
    
    private static String keyString(String menu, String label) {
        return menu + ": " + label;
    }

    private static I makeI(String menuname, String label, ItemAction action, int barKinds) {
        String key = keyString(menuname, label);
        I i = new I(label, key, action, barKinds);
        itemtable.put(key,i);
        return i;
    }

    private static I indexMenuItem(M menu, String label, ItemAction action) {
        return indexMenuItem(menu, menu.nextIndex(), label, action, menu.barKinds);
    }

    private static I indexMenuItem(M menu, String label, ItemAction action, int barKinds) {
        return indexMenuItem(menu, menu.nextIndex(), label, action, barKinds);
    }
    
    private static I indexMenuItem(M menu, int index, String label, ItemAction action) {
        return indexMenuItem(menu, index, label, action, menu.barKinds);
    }
    
    private static I indexMenuItem(M menu, int index, String label, ItemAction action, int barKinds) {
        versionstamp++;
        I i = makeI(menu.title, label, action, barKinds); 
        menu.insert(index, i);
        return i;
    }
    
    private static int menumask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
    
    public static void addStdFileMenuItems(M filemenu) {
        if (LocalSettings.aboutMenuItemNeeded) {
            indexMenuItem(filemenu, "About jape", new AboutBoxAction());
            filemenu.addSep();
        }
        
        indexMenuItem(filemenu, "Open...", new OpenFileAction()).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_O, menumask));
        
        indexMenuItem(filemenu, "Open new theory...", new CmdAction("reset;reload"));
        
        filemenu.add(new RFM(filemenu.barKinds));

        indexMenuItem(filemenu, "Close", new CloseProofAction(), PROOFWINDOW_BAR).
             setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W, menumask));
        
        indexMenuItem(filemenu, "Save Proofs", new CmdAction("saveproofs false")).
             setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, menumask));
        
        indexMenuItem(filemenu, "Save Proofs As...", new CmdAction("saveproofs true"));

        filemenu.addSep();

        indexMenuItem(filemenu, "Erase theory", new CmdAction("reset"));

        filemenu.addSep();
        
        indexMenuItem(filemenu, "Text Command...", new TextCommandAction());
        
        filemenu.addSep(PROOFWINDOW_BAR);
        
        indexMenuItem(filemenu, "Page Setup...", new PageSetupAction(), PROOFWINDOW_BAR).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P,
                                                  menumask+java.awt.Event.SHIFT_MASK));
        indexMenuItem(filemenu, "Print...", new PrintProofAction(), PROOFWINDOW_BAR).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P, menumask));

        filemenu.addSep(PROOFWINDOW_BAR);
        
        indexMenuItem(filemenu, EXPORT, new ExportAction(), PROOFWINDOW_BAR);
        indexMenuItem(filemenu, EXPORT_PROOF, new ExportProofAction(), PROOFWINDOW_BAR).
                  setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_E,
                                                  menumask));
        indexMenuItem(filemenu, EXPORT_DISPROOF, new ExportDisproofAction(), PROOFWINDOW_BAR);

        filemenu.addSep();
        
        indexMenuItem(filemenu, "Font Sizes ...", new FontSizesAction());
        if (DebugVars.showDebugVars) {
            indexMenuItem(filemenu, "Debug Log Settings ...", new DebugSettingsAction());
        }
        
        if (LocalSettings.quitMenuItemNeeded) {
            filemenu.addSep();
            indexMenuItem(filemenu, "Quit", new QuitAction());
        }
    }
    
    public static void addStdEditMenuItems(M editmenu) {
        /* no Undo, Redo in text dialogs till I work out how to do it, sigh */
        
        indexMenuItem(editmenu, "Undo", new UndoAction(), PROOFWINDOW_BAR/*|TEXTDIALOGWINDOW_BAR*/).
        setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z, menumask));

        indexMenuItem(editmenu, "Redo", new RedoAction(), PROOFWINDOW_BAR/*|TEXTDIALOGWINDOW_BAR*/).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z,
                                                  menumask+java.awt.Event.SHIFT_MASK));
        
        editmenu.addSep(PROOFWINDOW_BAR/*|TEXTDIALOGWINDOW_BAR*/);

        indexMenuItem(editmenu, "Apply tactic", new TacticCommandAction(), PROOFWINDOW_BAR/*|TEXTDIALOGWINDOW_BAR*/).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_T,
                                                  menumask));

        if (lemmasAllowed) {
            editmenu.addSep(PROOFWINDOW_BAR);
            indexMenuItem(editmenu, MAKE_LEMMA, new MakeLemmaAction(), PROOFWINDOW_BAR);
        }
        
        editmenu.addSep(PROOFWINDOW_BAR/*|TEXTDIALOGWINDOW_BAR*/);

        indexMenuItem(editmenu, "Cut", new UnimplementedAction("Edit: Cut"), TEXTDIALOGWINDOW_BAR).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_X, menumask));

        indexMenuItem(editmenu, COPY, new CopyUnicodeAction(), PROOFWINDOW_BAR|TEXTDIALOGWINDOW_BAR).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_C, menumask));

        indexMenuItem(editmenu, "Copy Proof", new CopyProofAction(), PROOFWINDOW_BAR).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_C,
                                                  menumask+java.awt.Event.ALT_MASK));
        
        indexMenuItem(editmenu, "Paste", new UnimplementedAction("Edit: Paste"), TEXTDIALOGWINDOW_BAR).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_V, menumask));

        indexMenuItem(editmenu, "Clear", new UnimplementedAction("Edit: Clear"), TEXTDIALOGWINDOW_BAR);

        indexMenuItem(editmenu, "Select All", new UnimplementedAction("Edit: Select All"), TEXTDIALOGWINDOW_BAR).
            setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A, menumask));
        
        if (LocalSettings.prefsMenuItemNeeded) {
            editmenu.addSep();
            indexMenuItem(editmenu, "Preferences...", new PrefsAction(), ALL_BARS);
        }
        
    }

    private static void addStdWindowMenuItems(M windowmenu) {
        // just put surviving items in itemtable
        if (DebugVars.menuaction_tracing)
            Logger.log.println("addStdWindowMenuItems "+windowmenu.size()+
                            "; "+windowmenu);
        for (int i=0; i<windowmenu.size(); i++) {
            Object o = windowmenu.get(i);
            if (o instanceof I) {
                I item = (I)o;
                itemtable.put(item.key, item);
            }
            else
            if (o instanceof Sep) { }
            else
                Alert.abort("JapeMenu.addStdWindowMenuItems sees "+o);
        }
    }
    
    /* This is a first, bad, approximation: we should have tooltips! */
    
    private static void addStdHelpMenuItems(M helpmenu) {
        if (Jape.onMacOSX)
           indexMenuItem(helpmenu, "Explain help",
                         new AlertAction("Type terms in the Search box at the top of the help menu.\nTheory-specific help may be available in proof windows."),
                         ALL_BARS);
        else
           indexMenuItem(helpmenu, "Explain help",
                        new AlertAction("There is no generic help for the interface; but theory-specific help may be available in proof windows."),
                        ALL_BARS);
    }
    
    public static void newMenu(String s, int barKinds) throws ProtocolError {
        indexMenu(s, barKinds);
    }
    
    // ActionListener interface (for menus)
    public static boolean checkboxDoubleBounce = false; // calamity: see Jape.main
    
    protected static class MenuItemListener implements ActionListener {
        private Window window;
        public MenuItemListener(Window w) {
            this.window = w;
        } 
        public void actionPerformed(ActionEvent newEvent) {
            if (DebugVars. menuaction_tracing)
                Logger.log.println("JapeMenu.actionPerformed "+this+" "+newEvent);
            String key = newEvent.getActionCommand();
            if (DebugVars. menuaction_tracing)
                Logger.log.println("key "+JapeUtils.enQuote(key)+
                                   "; indexes "+itemtable.get(key));
            I i = (I)itemtable.get(key);
            if (i!=null) {
                if (checkboxDoubleBounce && i instanceof CB) {
                    CB cb = (CB)i;
                    if (DebugVars. menuaction_tracing)
                        Logger.log.println("CB "+cb.ready);
                    cb.ready = !cb.ready;
                    if (!cb.ready)
                        return;
                }
                i.action.action(window);
            }
            else
                Alert.showErrorAlert("unrecognised menu action "+key);
        }
    }

    private static boolean menusVisible = false;

    public static void initMenuBar() {
        // this is the reset action, too
        menutable = new Hashtable<String, M>(20,(float)0.5);
        itemtable = new Hashtable<String, I>(100,(float)0.5);
        barv = new Vector<M>();

        addStdFileMenuItems(indexMenu("File", UNDIALOG_BARS));
        addStdEditMenuItems(indexMenu("Edit", EDIT_BARS));
        addStdWindowMenuItems(windowmenu==null ? (windowmenu=indexMenu("Window", UNDIALOG_BARS)) :
                                                 indexMenu(windowmenu, "Window"));
        addStdHelpMenuItems(indexMenu("Help", ALL_BARS));

        if (recentFiles==null)
            setRecentFiles(FilePrefs.getRecentFiles(), false);
        
        menusVisible = false;
    }

    public static void cancelMenus() {
        initMenuBar(); // I hope
    }

    private static boolean lemmasAllowed = false;
    
    public static void emptyMenus(boolean la) {
        lemmasAllowed = la;
        initMenuBar(); // I hope
    }
    
    public static void enableLemmas(boolean enable) {
        if (lemmasAllowed) {
            try {
                enableItem(true, "Edit", MAKE_LEMMA, JapeMenu.PROOFWINDOW_BAR, enable);
            } catch (ProtocolError e) {
                Alert.abort("JapeMenu.enableLemmas can't");
            }       
        }
    }

    public static void makeMenusVisible() {
        if (!menusVisible) {
            JapeWindow.updateMenuBars();
            menusVisible = true;
        }
    }

    private static M windowmenu;
    private static boolean hassurrogate=false, haslog = false;
    private static int panelcount=0, proofcount=0;

    /* the place where the Window menu is organised.
        It has several sections, all of which (except the last) may be empty:
            the surrogate;
            the proofs;
            the panels;
            the log window.
     */

    private static int alphapos(M menu, String title, int start) {
        for (int i=start; i<menu.size(); i++) {
            Object o = menu.get(i);
            if (o instanceof Sep)
                return i;
            else
            if (o instanceof I) {
                int order = title.compareTo(((I)o).label);
                if (order<0)
                    return i;
                else
                if (order==0) { // this happens when panels are resized
                    menu.removeI(title);
                    return i;
                }
            }
        }
        return menu.size();
    }
    
    /*  the Window menu consists of four lexically ordered sections:
        0.  surrogates 
        1.  panels
        2.  proofs
        3.  logs
        There is either zero or one surrogate. There is always one log.
        Sections are separated (with a separator!).
     */
    
    public static void windowAdded(String title, JapeWindow w) {
        if (w instanceof SurrogateWindow) {
            if (hassurrogate)
                Alert.abort("JapeMenu.addWindow two surrogates");
            else {
                hassurrogate = true;
                indexMenuItem(windowmenu, 0, title, (ItemAction)new ActivateWindowAction(w));
                windowmenuSep(1);
            }
        }
        else
        if (w instanceof PanelWindowData.PanelWindow) {
            int offset = hassurrogate?2:0;
            indexMenuItem(windowmenu, 
                          panelcount==0 ? offset : alphapos(windowmenu, title, offset),
                          title, 
                          (ItemAction)new ActivateWindowAction(w));
            panelcount++;
            windowmenuSep(offset+panelcount);
        }
        else
        if (w instanceof ProofWindow) {
            int offset = (hassurrogate?2:0)+(panelcount==0?0:panelcount+1); 
            indexMenuItem(windowmenu, 
                          proofcount==0 ? offset : alphapos(windowmenu, title, offset),
                          title, 
                          (ItemAction)new ActivateWindowAction(w));
            proofcount++;
            windowmenuSep(offset+proofcount);
        }
        else
        if (w instanceof Logger.LogWindow) {
            if (haslog)
                Alert.abort("JapeMenu.addWindow two console logs");
            else {
                haslog = true;
                indexMenuItem(windowmenu, windowmenu.size(), title,
                              (ItemAction)new ShowWindowAction(w));
            }
        }
        else
            Alert.abort("JapeMenu.addWindow "+w);

        if (menusVisible)
            JapeWindow.updateMenuBars();
    }

    private static void windowmenuSep(int i) {
        if (i==windowmenu.size() || !(windowmenu.get(i) instanceof Sep))
            windowmenu.insertSep(i);
    }
    
    public static void windowActivated(String title, JapeWindow w) {
        if (LocalSettings.windowMenuItemsTicked) {
            try {
                tickItem(false, "Window", title, ALL_BARS, true);
            } catch (ProtocolError e) {
                Alert.abort("JapeMenu.windowActivated "+JapeUtils.enQuote(title)+"; windowmenu="+windowmenu);
            }
        }
    }
        
    public static void windowRemoved(String title, JapeWindow w) {
        // Logger.log.println("JapeMenu.windowRemoved("+JapeUtils.enQuote(title)+","+w+")");
        windowmenu.removeI(title);
        if (w instanceof SurrogateWindow && hassurrogate) {
            hassurrogate = false; 
            windowmenu.removeSep(0);
        }
        else
        if (w instanceof PanelWindowData.PanelWindow && panelcount>0) {
            if (--panelcount==0) {
                windowmenu.removeSep(hassurrogate ? 2 : 0);
            }
        }
        else
        if (w instanceof ProofWindow && proofcount>0) {
            if (--proofcount==0) {
                windowmenu.removeSep((hassurrogate ? 2 : 0)+(panelcount==0?0:panelcount+1));
            }
        }
        else
        if (w instanceof Logger.LogWindow) {
            haslog = false; 
            windowmenu.removeSep(windowmenu.size()-1);
        }
        else
            Alert.abort("JapeMenu.windowRemoved("+JapeUtils.enQuote(title)+","+windowmenu+
                        "; hassurrogate="+hassurrogate+
                        "; panelcount="+panelcount+"; proofcount="+proofcount);

        if (menusVisible)
            JapeWindow.updateMenuBars();
    }
    
    public static void addSeparator(String menuname) throws ProtocolError {
        M menu = ensureMenu(menuname);
        addSeparator(menuname, menu.barKinds);
    }
    
    public static void addSeparator(String menuname, int barKinds) throws ProtocolError {
        M menu = ensureMenu(menuname);
        menu.addSep(barKinds);
    }
    
    public static void addItem(String menuname, String label, String code, String cmd)
        throws ProtocolError {
            M menu = ensureMenu(menuname);
            addItem(menuname, label, code, cmd, menu.barKinds);
        }
    
    public static void addItem(String menuname, String label, String code, String cmd, int barKinds)
        throws ProtocolError {
            M menu = ensureMenu(menuname);
            I item = (I)itemtable.get(keyString(menuname, label));
            ItemAction action = new CmdAction(cmd);
            if (item==null)
                item = indexMenuItem(menu, label, action, barKinds);
            else
                item.action = action;
            item.setAccelerator((code.equals(" ") || code.length()!=1) ? null :
                                KeyStroke.getKeyStroke((int)code.charAt(0), menumask));
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
                    if (true || DebugVars.menuaction_tracing)
                        Logger.log.println("no item "+label+" in menu "+menuname+" in window "+w);
                }
                else
                if (DebugVars. menuaction_tracing)
                    Logger.log.println("no menu "+menuname+" in window "+w);
            }
        }
    }
    
    public static void enableItem(boolean focussedonly, final String menuname, final String label,
                                  final int barKinds, final boolean enable) throws ProtocolError {
        if (menuname.equals("Edit") &&
            (label.startsWith("Undo") || label.startsWith("Redo")) &&
            label.indexOf("Step")!=-1) {
            ProofWindow.setHistoryVar(label.startsWith("Undo"), label.indexOf("Proof")!=-1, enable);
        }
        else try {
            final M menu = ensureMenu(menuname);
            final I action = menu.findI(label);
            
            /* we do this once in the data structure */
            if (!focussedonly)
                action.setEnabled(enable);
            
            /* and then again in the live menus */
            if (focussedonly) {
                if ((barKinds & PROOFWINDOW_BAR)!=0)
                    doEnableItem(ProofWindow.maybeFocussedWindow(), menuname, label, enable);
            }
            else
                JapeWindow.windowIter(new JapeWindow.WindowAction() {
                    public void action(JapeWindow w) {
                        if ((w.getBarKind() & barKinds)!=0)
                            doEnableItem(w, menuname, label, enable);
                    }
                });
        }  catch (ProtocolError e) {
                if (menuname.equals("Edit") && label.equals("Disprove"))
                    return;
                else
                    throw e;
        }
    }
        
    public static void addRadioButtonGroup(String menuname, String[][] rbs) 
        throws ProtocolError {
            M menu = ensureMenu(menuname);
            addRadioButtonGroup(menuname, rbs, menu.barKinds);
        }
    
    public static void addRadioButtonGroup(String menuname, String[][] rbs, int barKinds) 
        throws ProtocolError {
            M menu = ensureMenu(menuname);
            I[] rbg = new I[rbs.length];
            for (int i=0; i<rbs.length; i++)
                rbg[i] = makeI(menuname, rbs[i][0], new CmdAction(rbs[i][1]), barKinds);
            menu.add(new RBG(rbg, barKinds));
        }
    
    public static void addCheckBox(String menuname, String label, String cmd)
        throws ProtocolError {
        M menu = ensureMenu(menuname);
        addCheckBox(menuname, label, cmd, menu.barKinds);
    }
    
    public static void addCheckBox(String menuname, String label, String cmd, int barKinds)
        throws ProtocolError {
        M menu = ensureMenu(menuname);
        String key = keyString(menuname, label);
        CB cb = new CB(label, key, new CmdAction(cmd), barKinds);
        itemtable.put(key,cb);
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
                    if (true || DebugVars. menuaction_tracing)
                        Logger.log.println("no item "+label+" in menu "+menuname+" in window "+w);
                }
                else
                if (DebugVars. menuaction_tracing)
                    Alert.showAlert(Alert.Warning, "no menu "+menuname+" in window "+w);
            }
        }
    }
    
    public static void tickItem(boolean focussedonly, final String menuname, final String label,
                                final int barKinds, final boolean state)
        throws ProtocolError {
        M menu = ensureMenu(menuname);
        I item = menu.findI(label);

        item.setSelected(state);

        if (focussedonly) {
            if ((barKinds & PROOFWINDOW_BAR)!=0) 
                doTickItem(ProofWindow.maybeFocussedWindow(), menuname, label, state);
        }
        else
            JapeWindow.windowIter(new JapeWindow.WindowAction() {
                public void action(JapeWindow w) {
                    if ((w.getBarKind() & barKinds)!=0)
                        doTickItem(w, menuname, label, state);
                }
            });
    }
    
    private static Vector<String> recentFiles = null;
        
    public static void setRecentFiles(String recent, boolean rebuildMenus) {
        recentFiles = new Vector<String>();
        int i=0, j;
        while ((j=recent.indexOf("\n", i))!=-1) {
            String next = recent.substring(i, j);
            recentFiles.add(next);
            java.io.File file = new java.io.File(next);
            String name = file.getName();
            String parent = file.getParent();
            recentFiles.add(parent==null ? name : name+" - "+parent);
            i = j+1;
        }
        if (rebuildMenus && menusVisible)
            JapeWindow.updateMenuBars();
    }
        
    private static JMenu recentFilesMenu() {
        JMenu menu = new JMenu(OPEN_RECENT);
        
        if (recentFiles.size()!=0) {
            for (int i=0; i<recentFiles.size(); i+=2) {
                final String f = (String)recentFiles.get(i);
                String text = (String)recentFiles.get(i+1);
                JMenuItem item = new JMenuItem(text);
                item.setActionCommand(text);
                item.setEnabled(true);
                item.setSelected(false);
                JapeFont.setComponentFont(item.getComponent(), JapeFont.MENUENTRY);
                if (DebugVars.menuaction_tracing)
                    Logger.log.println("addRecentFile "+JapeUtils.enQuote(item.getText()));
                item.addActionListener(new ActionListener(){
                    public void actionPerformed(ActionEvent newEvent) {
                        FilePrefs.recordRecentFile(f);
                        if (Jape.onUnix && f.startsWith("~")) {
                            String home = System.getProperties().getProperty("user.home");
                            String g = home+f.substring(1);
                            doOpenFile(g);
                        }
                        else {
                            doOpenFile(f);
                        }
                    }
                });
                menu.add(item);
            }
            menu.addSeparator();
            JMenuItem clear = new JMenuItem("Clear Menu");
            clear.setEnabled(true);
            clear.setSelected(false);
            JapeFont.setComponentFont(clear.getComponent(), JapeFont.MENUENTRY);
            if (DebugVars.menuaction_tracing)
                Logger.log.println("addRecentFile "+JapeUtils.enQuote(clear.getText()));
            clear.addActionListener(new ActionListener(){
                public void actionPerformed(ActionEvent newEvent) {
                    FilePrefs.clearRecentFiles();
                }
            });
            menu.add(clear);
        }
        
        return menu;
    }
}



