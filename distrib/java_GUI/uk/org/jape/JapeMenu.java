/* 
    $Id$

    Copyright © 2003-4 Richard Bornat & Bernard Sufrin
     
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

import java.awt.Font;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Toolkit;

import java.awt.datatransfer.StringSelection;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.KeyStroke;
import javax.swing.UIManager;

public class JapeMenu implements DebugConstants {
    
    /*	Java can't share menu bars or menus or menu items, because of the
	containment hierarchy idea.

	So this is a menu bar / menu / menu item factory.

	It seems to get very confused when allocating menu bars quite often
	(which happens because of the Window menu).  So I'm using version stamps
	to try to cut down the traffic ...
     */

    private static int versionstamp = 0;
    
    private static Vector barv = new Vector(); // of Ms
    
    protected static class M {
	final boolean proofsonly; final String title;
	protected final Vector itemv=new Vector(); // of Is and Seps and RBGs and CBs
	M(boolean proofsonly, String title) { this.proofsonly=proofsonly; this.title=title; }
	private static final I quitItem = new I("Quit", "", null),
			       doneItem = new I("Done", "", null);
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
		return	size;
	}
	public void add(I i) { itemv.insertElementAt(i,nextIndex()); }
	public void insert(int index, I i) { itemv.insertElementAt(i,index); }
	public void add(RBG rbg) { itemv.insertElementAt(rbg,nextIndex()); }
	public void addSep() { insertSep(nextIndex()); }
	public void insertSep(int i) {
	    if (preSep(i))
		itemv.insertElementAt(new Sep(),i);
	}
	public void removeI(String label) {
	    int vc = itemv.size();
	    for (int i=0; i<vc; i++) {
		Object o = itemv.get(i);
		if (o instanceof I && ((I)o).label.equals(label)) {
		    itemv.remove(i); return;
		}
	    }
	    Alert.abort("JapeMenu.M.removeI \""+label+"\" from "+this);
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
	    throw new ProtocolError("no item \""+label+"\" in menu "+title);
	}
	public boolean preSep(int i) {
	    return 0<i && i<=itemv.size() && !(itemv.get(i-1) instanceof Sep);
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
	final String label;
	final String key;
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

    public static final String PAGE_SETUP      = "Page Setup...",
			       PRINT_PROOF     = "Print...",
			       EXPORT	       = "Export",
			       EXPORT_PROOF    = "Export Proof",
			       EXPORT_DISPROOF = "Export Disproof";
    
    protected static TitledMenuBar mkBar(boolean isProofBar, JapeWindow w) {
	Object radioIcon = UIManager.get(RADIO_ICON_KEY);
	Object checkIcon = UIManager.get(CHECK_ICON_KEY);
	TitledMenuBar bar = new TitledMenuBar();
	ActionListener listener = new MenuItemListener(w);
	for (Enumeration ebar = barv.elements(); ebar.hasMoreElements(); ) {
	    M m = (M)ebar.nextElement();
	    if (!m.proofsonly || isProofBar) {
		TitledMenu menu = new TitledMenu(m.title);
		boolean isWindowMenu = LocalSettings.windowMenuItemsTicked && m.title.equals("Window");
		ButtonGroup buttonGroup = isWindowMenu ? new ButtonGroup() : null;
		if (isWindowMenu && radioIcon!=null && checkIcon!=null) {
		    // make RadioButtons which look like CheckBoxes
		    UIManager.put(RADIO_ICON_KEY, checkIcon);
		}
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
			    mkItem(menu, rbg.items[j], item, listener);
			    group.add(item);
			}
			if (m.postSep(i))
			    menu.addSeparator();
		    }
		    else
		    if (o instanceof CB) {
			CB cb = (CB)o;
			JCheckBoxMenuItem item = new JCheckBoxMenuItem(cb.label);
			mkItem(menu, cb, item, listener);
		    }
		    else
		    if (o instanceof I) {
			I ii = (I)o;
			if (m.title.equals("File") && ii.label.equals("Close"))
			    menu.add(recentFilesMenu());
			if (isProofBar || !m.title.equals("File") ||
			      (!(ii.label.equals("Close") ||
				 ii.label.equals(PAGE_SETUP) ||
				 ii.label.equals(PRINT_PROOF) ||
				 ii.label.equals(EXPORT) ||
				 ii.label.equals(EXPORT_PROOF) ||
				 ii.label.equals(EXPORT_DISPROOF)))) {
			    if (m.title.equals("File") && (ii.label.equals(PAGE_SETUP)	||
							   ii.label.equals(EXPORT)))
				menu.addSeparator();
			    if (buttonGroup==null) {
				JMenuItem item = new JMenuItem(ii.label);
				mkItem(menu, ii, item, listener);
			    }
			    else {
				JRadioButtonMenuItem item =
				   new JRadioButtonMenuItem(ii.label, i==0);
				if (DebugVars.menuaction_tracing)
				    Logger.log.println("window menu item "+ii.label+
						       "; window="+w.title);
				mkItem(menu, ii, item, listener);
				buttonGroup.add(item);
			    }
			}
		    }
		    else
		    if (o instanceof Sep)
			menu.addSeparator();
		    else
			Alert.abort("JapeMenu.mkBar sees "+o);
		}
		if (isWindowMenu && radioIcon!=null && checkIcon!=null)
		    UIManager.put(RADIO_ICON_KEY, radioIcon);
		bar.add(menu);
	    }
	}
	return bar;
    }

    private static void setJMenuBar(boolean isProofBar, JapeWindow w) {
	if (DebugVars.menuaction_tracing)
	    Logger.log.println("JapeMenu.setJMenuBar "+isProofBar+" \""+w.title+"\"");
	w.setJMenuBar(mkBar(isProofBar, w));
	w.getJMenuBar().revalidate();
    }

    public static int setBar(JapeWindow w, int stamp) {
	if (DebugVars.menuaction_tracing)
	    Logger.log.println("JapeMenu.setBar "+JapeUtils.enQuote(w.title)+
			       " "+stamp+
			       " ("+versionstamp+"; "+w.getJMenuBar()==null+")");
	if (w.getJMenuBar()==null || stamp<versionstamp) {
	    if (w instanceof ProofWindow)
		setJMenuBar(true, w);
	    else
	    if ((w instanceof PanelWindowData.PanelWindow && LocalSettings.panelWindowMenus) ||
		w instanceof SurrogateWindow || w instanceof Logger.LogWindow)
		setJMenuBar(false, w);
	}
	return versionstamp; 
    }
    
    // I need dictionaries from menu titles to Ms and item keys to Is: 
    // hashtables are overkill, but there you go -- so much of Java is.
    
    private static Hashtable menutable, itemtable;

    private static M ensureMenu(String menuname) throws ProtocolError {
	M menu = (M)menutable.get(menuname);
	if (menu==null)
	    throw new ProtocolError("no menu named "+menuname);
	return menu;
    }

    /* ******************************* menu actions ******************************* */

    private abstract static class ItemAction {
	abstract public void action(JapeWindow w);
    }

    private static class AboutBoxAction extends ItemAction {
	public void action(JapeWindow w) { Jape.handleAbout(); }
    }

    private static class ActivateWindowAction extends ItemAction {
	JapeWindow w;
	public ActivateWindowAction(JapeWindow w) {
	    super(); this.w=w;
	}
	public void action(JapeWindow ignore) {
	    w.toFront();
	}
    }

    private static class AlertAction extends ItemAction {
	String s;
	AlertAction (String s) { this.s = s; }
	public void action (JapeWindow w) {  Alert.showAlert(Alert.Warning, s); }
    }

    private static class CloseProofAction extends ItemAction {
	public void action(JapeWindow w) {
	    if (w instanceof ProofWindow)
		((ProofWindow)w).closeProof();
	    else
		Alert.abort("CloseProofAction on non-proof window");
	}
    }

    private static class CmdAction extends ItemAction {
	String cmd;
	CmdAction (String cmd) { this.cmd = cmd; }
	public void action (JapeWindow w) {
	     Reply.sendCOMMAND(cmd);
	}
    }

    private static class CopyProofAction extends ItemAction {
	public void action (JapeWindow w) {
	    Alert.showAlert(Alert.Info,
			    "Copy Proof doesn't work yet -- but Export (see File menu) can make a pdf file");
	}
    }

    private static class CopyUnicodeAction extends ItemAction {
	public void action (JapeWindow w) {
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
	public void action (JapeWindow w) {
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
	public void action (JapeWindow w) {
	    DebugVars.runDebugSettingsDialog();
	}
    }
    
    private static class ExportAction extends ItemAction {
	public void action(JapeWindow w) {
	    if (w instanceof ProofWindow)
		Export.export((ProofWindow)w, PrintProof.BOTH);
	    else
		Alert.abort("ExportAction on non-proof window");
	}
    }
    
    private static class ExportDisproofAction extends ItemAction {
	public void action(JapeWindow w) {
	    if (w instanceof ProofWindow)
		Export.export((ProofWindow)w, PrintProof.DISPROOF);
	    else
		Alert.abort("ExportDisproofAction on non-proof window");
	}
    }
    
    private static class ExportProofAction extends ItemAction {
	public void action(JapeWindow w) {
	    if (w instanceof ProofWindow)
		Export.export((ProofWindow)w, PrintProof.PROOF);
	    else
		Alert.abort("ExportProofAction on non-proof window");
	}
    }
    
    private static class FontSizesAction extends ItemAction {
	public void action (JapeWindow w) {
	    JapeFont.runFontSizesDialog();
	}
    }

    private static class HideWindowAction extends ItemAction {
	JFrame w;
	public HideWindowAction(JFrame w) {
	    super(); this.w=w;
	}
	public void action(JapeWindow ignore) {
	    w.setVisible(false);
	}
    }

    private static class OpenFileAction extends ItemAction {
	public void action (JapeWindow w) {
	  doOpenFile(chooseFile());
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
	    Reply.sendCOMMAND("use \""+file+"\"");

    }

    private static class PrefsAction extends ItemAction {
	public void action(JapeWindow w) { Jape.handlePrefs(); }
    }

    private static class PageSetupAction extends ItemAction {
	public void action(JapeWindow w) { PrintProof.pageSetup(); }
    }

    private static class PrintProofAction extends ItemAction {
	public void action(JapeWindow w) {
	    if (w instanceof ProofWindow)
		PrintProof.printProof((ProofWindow)w);
	    else
		Alert.abort("PrintProofAction on non-proof window");
	}
    }
    
    private static class RedoAction extends ItemAction {
	public void action(JapeWindow w) {
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
	public void action(JapeWindow ignore) {
	    w.setVisible(true);
	    w.toFront();
	}
    }
    
    private static class TextCommandAction extends ItemAction {
	public void action(JapeWindow w) { TextDialog.runTextCommandDialog(); }
    }
    
    
    private static class UndoAction extends ItemAction {
	public void action(JapeWindow w) {
	    if (w instanceof ProofWindow)
		Reply.sendCOMMAND("undo_"+((ProofWindow)w).undoSuffix());
	    else
		Alert.abort("UndoAction not in ProofWindow");
	}
    }
    
    private static class QuitAction extends ItemAction {
	public void action(JapeWindow w) { Jape.handleQuit(); }
    }

    private static class UnimplementedAction extends ItemAction {
	String s;
	UnimplementedAction (String s) { super(); this.s = s; }
	public void action (JapeWindow w) {  Logger.log.println(s); }
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

    private static M indexMenu(boolean proofsonly, String label) {
	M menu = (M)menutable.get(label);
	if (menu==null) {
	    return indexMenu(new M(proofsonly, label), label);
	}
	return menu;
    }
    
    private static String keyString(String menu, String label) {
	return menu + ": " + label;
    }

    private static I makeI(String menuname, String label, ItemAction action) {
	String key = keyString(menuname, label);
	I i = new I(label, key, action);
	itemtable.put(key,i);
	return i;
    }

    private static I indexMenuItem(M menu, String label, ItemAction action) {
	return indexMenuItem(menu, menu.nextIndex(), label, action);
    }

    private static I indexMenuItem(M menu, int index, String label, ItemAction action) {
	versionstamp++;
	I i = makeI(menu.title, label, action); 
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

	indexMenuItem(filemenu, "Close", new CloseProofAction()).
	     setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W, menumask));
	
	indexMenuItem(filemenu, "Save Proofs", new CmdAction("saveproofs false")).
	     setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, menumask));
	
	indexMenuItem(filemenu, "Save Proofs As...", new CmdAction("saveproofs true"));

	filemenu.addSep();

	indexMenuItem(filemenu, "Erase theory", new CmdAction("reset"));

	filemenu.addSep();
	
	indexMenuItem(filemenu, "Text Command...", new TextCommandAction());
	
	// separator implicit before these ...
	
	indexMenuItem(filemenu, PAGE_SETUP, new PageSetupAction()).
	    setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P,
						  menumask+java.awt.Event.SHIFT_MASK));
	indexMenuItem(filemenu, PRINT_PROOF, new PrintProofAction()).
	    setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P, menumask));
	indexMenuItem(filemenu, EXPORT, new ExportAction());
	indexMenuItem(filemenu, EXPORT_PROOF, new ExportProofAction());
	indexMenuItem(filemenu, EXPORT_DISPROOF, new ExportDisproofAction());

	filemenu.addSep();
	
	indexMenuItem(filemenu, "Font Sizes ...", new FontSizesAction());
	if (DebugVars.showDebugVars) {
	    indexMenuItem(filemenu, "Debug Settings ...", new DebugSettingsAction());
	}
	
	if (LocalSettings.quitMenuItemNeeded) {
	    filemenu.addSep();
	    indexMenuItem(filemenu, "Quit", new QuitAction());
	}
    }
    
    public static void addStdEditMenuItems(M editmenu) {
	indexMenuItem(editmenu, "Undo", new UndoAction()).
	setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z, menumask));

	indexMenuItem(editmenu, "Redo", new RedoAction()).
	    setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z,
						  menumask+java.awt.Event.SHIFT_MASK));

	editmenu.addSep();

	/* we don't need Cut in proofs (we do in text dialogs, but they are to come)
	    indexMenuItem(editmenu, "Cut", new UnimplementedAction("Edit: Cut")).
		setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_X, menumask));
	 */

	indexMenuItem(editmenu, "Copy", new CopyUnicodeAction()).
	    setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_C, menumask));

	/*  indexMenuItem(editmenu, "Copy Ascii", new CopyAsciiAction()).
		setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_C,
						      menumask+java.awt.Event.SHIFT_MASK));
	 */
	
	indexMenuItem(editmenu, "Copy Proof", new CopyProofAction()).
	    setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_C,
						  menumask+java.awt.Event.ALT_MASK));
	
	/* likewise we don't need Paste, Clear or Select All ...
	indexMenuItem(editmenu, "Paste", new UnimplementedAction("Edit: Paste")).
	    setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_V, menumask));

	indexMenuItem(editmenu, "Clear", new UnimplementedAction("Edit: Clear"));

	indexMenuItem(editmenu, "Select All", new UnimplementedAction("Edit: Select All")).
	    setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A, menumask));
	 */
	
	if (LocalSettings.prefsMenuItemNeeded) {
	    editmenu.addSep();
	    indexMenuItem(editmenu, "Preferences...", new PrefsAction());
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
    
    private static void addStdHelpMenuItems(M helpmenu) {
	indexMenuItem(helpmenu, "No help yet",
		      new AlertAction("GUI doesn't have any help available yet.\n\nSorry."));
    }
    
    public static void newMenu(boolean proofsonly, String s) throws ProtocolError {
	indexMenu(proofsonly, s);
    }
    
    // ActionListener interface (for menus)
    public static boolean checkboxDoubleBounce = false; // calamity: see Jape.main
    
    protected static class MenuItemListener implements ActionListener {
	private JapeWindow window;
	public MenuItemListener(JapeWindow w) {
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
	menutable = new Hashtable(20,(float)0.5);
	itemtable = new Hashtable(100,(float)0.5);
	barv = new Vector();

	addStdFileMenuItems(indexMenu(false, "File"));
	addStdEditMenuItems(indexMenu(true,  "Edit"));
	addStdWindowMenuItems(windowmenu==null ? (windowmenu=indexMenu(false, "Window")) :
						 indexMenu(windowmenu, "Window"));
	addStdHelpMenuItems(indexMenu(false, "Help"));

	if (recentFiles==null)
	    setRecentFiles(FilePrefs.getRecentFiles(), false);
	
	menusVisible = false;
    }

    public static void cancelMenus() {
	initMenuBar(); // I hope
    }

    public static void emptyMenus() {
	initMenuBar(); // I hope
    }

    public static void makeMenusVisible() {
	JapeWindow.updateMenuBars();
	menusVisible = true;
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
    
    public static void windowAdded(String title, JapeWindow w) {
	int insertpoint = -1, seppoint = -1;
	if (w instanceof SurrogateWindow) {
	    if (hassurrogate)
		Alert.abort("JapeMenu.addWindow two surrogates");
	    else {
		hassurrogate = true;
		insertpoint = 0; seppoint = 1;
	    }
	}
	else
	if (w instanceof PanelWindowData.PanelWindow) {
	    if (panelcount==0) {
		if (hassurrogate || proofcount!=0) { // surrogate or proofs but no panels
		    insertpoint = 1; seppoint = 1;
		}
		else { // no surrogate, no proofs, no panels
		    insertpoint = 0; seppoint = 1;
		}
	    }
	    else // insert in lexical ordering
		insertpoint = alphapos(windowmenu, title, hassurrogate?2:0);
	    panelcount++;
	}
	else
	if (w instanceof ProofWindow) {
	    if (proofcount==0) { // no proofs
		if (windowmenu.size()==0) // first in window
		    insertpoint = 0;
		else
		if (haslog) { // before logwindow, separator after
		    insertpoint = windowmenu.size()-1; seppoint = insertpoint+1;
		}
		else // end of window, separator before
		    insertpoint = seppoint = windowmenu.size();
	    }
	    else // insert in lexical ordering
		insertpoint = alphapos(windowmenu, title,
					(hassurrogate?2:0)+(panelcount==0?0:panelcount+1));
	    proofcount++;		     
	}
	else
	if (w instanceof Logger.LogWindow) {
	    if (haslog)
		Alert.abort("JapeMenu.addWindow two console logs");
	    else {
		haslog = true;
		insertpoint = seppoint = windowmenu.size();
	    }
	}
	else
	    Alert.abort("JapeMenu.addWindow "+w);

	if (insertpoint==-1) {
	    Logger.log.println("*** insertpoint=-1 when inserting "+w);
	    insertpoint = 0;
	}

	if (DebugVars.menuaction_tracing)
	    Logger.log.println("adding window "+title+
			       "; insertpoint="+insertpoint+"; seppoint="+seppoint+
			       "; menu size="+windowmenu.size());
	
	indexMenuItem(windowmenu, insertpoint, title,
		      w instanceof Logger.LogWindow ? (ItemAction)new ShowWindowAction(w)     :
						      (ItemAction)new ActivateWindowAction(w));
	
	if (seppoint!=-1)
	    windowmenu.insertSep(seppoint);

	if (menusVisible)
	    JapeWindow.updateMenuBars();
    }

    public static void windowActivated(String title, JapeWindow w) {
	if (LocalSettings.windowMenuItemsTicked) {
	    try {
		tickItem(false, "Window", title, true);
	    } catch (ProtocolError e) {
		Alert.abort("JapeMenu.windowActivated \""+title+"\"; windowmenu="+windowmenu);
	    }
	}
    }
	
    public static void windowRemoved(String title, JapeWindow w) {
	windowmenu.removeI(title);
	if (w instanceof SurrogateWindow && hassurrogate) {
	    hassurrogate = false; windowmenu.removeSep(0);
	}
	else
	if (w instanceof PanelWindowData.PanelWindow && panelcount>0) {
	    if (--panelcount==0) {
		if (hassurrogate)
		    windowmenu.removeSep(1);
		else
		if (proofcount!=0)
		    windowmenu.removeSep(0);
	    }
	}
	else
	if (w instanceof ProofWindow && proofcount>0) {
	    if (--proofcount==0) {
		if (hassurrogate || panelcount!=0)
		    windowmenu.removeSep(panelcount+(hassurrogate ? 2 : 0));
	    }
	}
	else
	if (w instanceof Logger.LogWindow) {
	    haslog = false; windowmenu.removeSep(windowmenu.size()-1);
	}
	else
	    Alert.abort("JapeMenu.windowRemoved(\""+title+"\","+windowmenu+
			"; hassurrogate="+hassurrogate+
			"; panelcount="+panelcount+"; proofcount="+proofcount);

	if (menusVisible)
	    JapeWindow.updateMenuBars();
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
	I item = (I)itemtable.get(keyString(menuname, label));
	ItemAction action = new CmdAction(cmd);
	if (item==null)
	    item = indexMenuItem(menu, label, action);
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
		    if (true || DebugVars. menuaction_tracing)
			Logger.log.println("no item "+label+" in menu "+menuname+" in window "+w);
		}
		else
		if (DebugVars. menuaction_tracing)
		    Logger.log.println("no menu "+menuname+" in window "+w);
	    }
	}
    }
    
    public static void enableItem(boolean focussedonly, final String menuname, final String label,
				  final boolean enable) throws ProtocolError {
	if (menuname.equals("Edit") &&
	    (label.startsWith("Undo") || label.startsWith("Redo")) &&
	    label.indexOf("Step")!=-1) {
	    ProofWindow.setHistoryVar(label.startsWith("Undo"), label.indexOf("Proof")!=-1, enable);
	}
	else
	try {
	    final M menu = ensureMenu(menuname);
	    final I action =menu.findI(label);

	    if (!focussedonly)
		action.setEnabled(enable);

	    if (focussedonly)
		doEnableItem(ProofWindow.maybeFocussedWindow(), menuname, label, enable);
	    else
		JapeWindow.windowIter(new JapeWindow.WindowAction() {
		    public void action(JapeWindow w) {
			doEnableItem(w, menuname, label, enable);
		    }
		});
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
				final boolean state)
	throws ProtocolError {
	M menu = ensureMenu(menuname);
	I item = menu.findI(label);

	item.setSelected(state);

	if (focussedonly)
	    doTickItem(ProofWindow.maybeFocussedWindow(), menuname, label, state);
	else
	    JapeWindow.windowIter(new JapeWindow.WindowAction() {
		public void action(JapeWindow w) {
		    doTickItem(w, menuname, label, state);
		}
	    });
    }
    
    private static Vector recentFiles = null;
	
    public static void setRecentFiles(String recent, boolean rebuildMenus) {
	recentFiles = new Vector();
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
	JMenu menu = new JMenu("Open Recent");
	
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
			    System.out.println("opening (~="+JapeUtils.enQuote(home)+
					       ") recent "+JapeUtils.enQuote(f)+
					       " => "+JapeUtils.enQuote(g));
			    doOpenFile(g);
			}
			else {
			    System.out.println("opening recent "+JapeUtils.enQuote(f));
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

