/* 
    $Id$

    Copyright © 2003-5 Richard Bornat & Bernard Sufrin
     
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

/* We mustn't create panel windows -- which means things like calculating
   preferred size and such -- until we have all the data we need.
 */
 
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.LayoutManager;
import java.awt.Point;
import java.awt.Rectangle;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.JViewport;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;

import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

public class PanelWindowData implements DebugConstants, ProtocolConstants {
    protected String title;
    protected int kind;
    PanelWindow window;
    
    Vector entryv, cmdv, markv; // matches the list line for line
    Vector buttonv;
    int selectedIndex = -1;
    
    private static final String newLabel       = "New...",
				proveLabel     = "Prove",
				showproofLabel = "Show Proof";
	
    public PanelWindowData(String title, int kind) {
	this.title = title; this.kind = kind;
	window = null;

	entryv = new Vector();
	cmdv = new Vector();
	markv = new Vector();
	buttonv = new Vector();
	
	if (kind==ConjecturePanelKind) { 
	    // has default buttons
	    buttonv.add(new NewButton());
	    addButton(proveLabel, 
		      new Insert[] { 
			  new StringInsert("proveconjecture"), new CommandInsert() }, true);
	    addButton(showproofLabel, 
		      new Insert[] { 
			  new StringInsert("showproof"), new CommandInsert() }, true);
	}
    }

    public boolean isDefaultButton(PanelButton b) {
	return isDefaultButton(b.label);
    }

    public boolean isDefaultButton(String text) {
	if (kind==ConjecturePanelKind) {
	    return text.equals("New...") || text.equals("Prove") || text.equals("Show Proof");
	}
	else
	    return false;
    }
    
    protected void addEntry(String entry, String cmd) {
	int i = entryv.indexOf(entry);
	if (i==-1) {
	    if (panellist_tracing)
		Logger.log.println("panel "+JapeUtils.enQuote(title)+" adding entry "+JapeUtils.enQuote(entry)+", cmd "+JapeUtils.enQuote(cmd));
	    entryv.add(entry);
	    cmdv.add(cmd);
	    markv.add(null);
	    if (window!=null)
		window.addEntry(entry, null);
	}
	else {
	    if (panellist_tracing)
		Logger.log.println("panel "+JapeUtils.enQuote(title)+" setting entry "+JapeUtils.enQuote(entry)+" to "+JapeUtils.enQuote(cmd));
	    cmdv.setElementAt(cmd, i);
	}
    }

    protected void markEntry(String cmd, boolean proved, boolean disproved) throws ProtocolError {
	    if (panellist_tracing)
		Logger.log.println("panel "+JapeUtils.enQuote(title)+" marking cmd "+JapeUtils.enQuote(cmd)+" "+proved+","+disproved);
	    int i = cmdv.indexOf(cmd);
	    if (i==-1)
		throw new ProtocolError("no such command");
	else {
	    markv.setElementAt(new Mark(proved,disproved) ,i);
	    if (window!=null)
		window.markEntry(i, proved, disproved);
	}
    }

    private static class Mark {
	public final boolean proved, disproved;
	public Mark(boolean proved, boolean disproved) {
	    this.proved = proved; this.disproved = disproved;
	}
    }

    protected void selectEntry(String entry)throws ProtocolError {
	if (panellist_tracing)
	    Logger.log.println("panel "+JapeUtils.enQuote(title)+" selecting "+entry);
	int i = entryv.indexOf(entry);
	if (i==-1)
	    throw new ProtocolError("no such entry");
	else {
	    selectedIndex = i;
	    if (window!=null)
		window.selectEntry(i);
	}
    }
    
    public static abstract class Insert { }
    public static class StringInsert extends Insert {
	String s;
	StringInsert(String s) { this.s=s; }
    }
    public static class LabelInsert extends Insert { }
    public static class CommandInsert extends Insert { }

    protected static abstract class PanelButton extends JButton {
	String label;
	PanelButton(String label) {
	    super(label);
	    this.label=label;
	    setActionCommand(label);
	}
	abstract boolean needsSelection();
	abstract void doAction(String title, int listIndex);
	public void font_reset() {
	    JapeFont.setComponentFont(this, JapeFont.PANELBUTTON);
	}
    }

    class InsertButton extends PanelButton {
	private Insert[] inserts;
	InsertButton(String label, Insert[] inserts) {
	    super(label);
	    setInserts(inserts);
	}
	public void setInserts(Insert[] inserts) {
	    this.inserts = inserts;
	}
	public boolean needsSelection() {
	    for (int i=0; i<inserts.length; i++) {
		Insert ins = inserts[i];
		if (ins instanceof CommandInsert || ins instanceof LabelInsert)
		    return true;
	    }
	    return false;
	}
	public void doAction(String panel, int listIndex) {
	    String res="";
	    for (int ci=0; ci<inserts.length; ci++) {
		if (ci!=0) res+=" ";
		res += inserts[ci] instanceof CommandInsert ? (String)cmdv.get(listIndex)      :
		       inserts[ci] instanceof LabelInsert   ? (String)entryv.get(listIndex)    :
							      ((StringInsert)inserts[ci]).s;
	    }
	    Reply.sendCOMMAND(res);
	}
    }

    class NewButton extends PanelButton {
	public NewButton() {
	    super(newLabel);
	}
	public boolean needsSelection() {
	    return false;
	}
	public void doAction(String panel, int listIndex) {
	    TextDialog.runNewConjectureDialog(panel);
	}
    }
    
    protected PanelButton findButton(String label) {
	for (int i=0; i<buttonv.size(); i++) {
	    PanelButton b = (PanelButton)buttonv.get(i);
	    if (b.label.equals(label))
		return b;
	}
	return null;
    }

    protected void addButton(String label, Insert[] inserts, boolean isDefault) {
	if (isDefaultButton(label)!=isDefault)
	    Alert.abort("PanelWindowData.addButton "+JapeUtils.enQuote(label)+" isDefault="+isDefault+
			" isDefaultButton(...)="+isDefaultButton(label));
	// if there already is such a button, just change its inserts
	PanelButton button = findButton(label);
	if (button!=null) {
	    if (button instanceof InsertButton)
		((InsertButton)button).setInserts(inserts);
	    else
		Alert.abort("PanelWindowData.addButton with inserts finds "+button);
	    return;
	}
	// otherwise a new one
	button = new InsertButton(label, inserts);
	buttonv.add(button);
	if (window!=null) {
	    if (panellayout_tracing)
		Logger.log.println("late PanelButton "+label);
	    window.addButton(button); // this is ok after an emptyPanel
	}
    }

    public void closeWindow() {
	if (window!=null) {
	    window.closeWindow();
	    window = null;
	}
    }

    public void emptyPanel() {
	if (panelempty_tracing)
	    Logger.log.println("emptying panel "+window.title);
	window.setVisible(false);
	window.model.removeAllElements();
	Component[] cs = window.getContentPane().getComponents();
	for (int i=0; i<cs.length; i++)
	    if (cs[i] instanceof PanelButton) {
		PanelButton b = (PanelButton)cs[i];
		if (!isDefaultButton(b)) {
		    if (panelempty_tracing)
			Logger.log.println("deleting button "+b.label);
		    window.removeButton(b);
		    buttonv.remove(buttonv.indexOf(b));
		}
	    }
	entryv.removeAllElements();
	cmdv.removeAllElements();
	markv.removeAllElements();
    }
    
    /* This is now basically functional, but things to do:
       1. Disable action of close button (and one day, if poss, grey out close button).
       2. Make panels float (maybe, and perhaps only on MacOS X)
       3. Make panels look different -- different title style? smaller title bar?
     */
 
    public class PanelWindow extends JapeWindow implements ActionListener {

	private final DefaultListModel model;
	private JList list;
	private JScrollPane scrollPane;
	private ButtonPane buttonPane;

	private int prefixw; // not final because it changes when font changes
	private boolean active = true;
	
	public PanelWindow() {
	    super(PanelWindowData.this.title);
    
	    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
	    addWindowListener(new WindowAdapter() {
		public void windowClosing(WindowEvent e) {
		    Alert.showAlert(PanelWindow.this, Alert.Info,
				    new String[] {
					"You can't close a Jape panel: it's needed!",
					"(and if only I knew how to grey out the close button ...)"
				    });
		}
		public void windowActivated(WindowEvent e) {
		    if (LocalSettings.showPanelWindowFocus) {
			active = true; repaintSelection();
		    }
		}
		public void windowDeactivated(WindowEvent e) {
		    if (LocalSettings.showPanelWindowFocus) {
			active = false; repaintSelection();
		    }
		}
	    });
    
	    Container contentPane = getContentPane();
	    contentPane.setLayout(new PanelWindowLayout());
    
	    model = new DefaultListModel();
	    list = new JList(model);
	    list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
	    setListFont();
	    list.setCellRenderer(new Renderer());

	    for (int i=0; i<entryv.size(); i++)
		addEntry((String)entryv.get(i), (Mark)markv.get(i));
	    list.setSelectedIndex(selectedIndex==-1 ? 0 : selectedIndex);

	    MouseListener m = new MouseAdapter () {
		public void mouseClicked(MouseEvent e) {
		    int index = list.locationToIndex(e.getPoint());
		    if (e.getClickCount()==2) {
			if (kind==ConjecturePanelKind && 0<=index && index<model.size())
			    // double-click means "prove this one"
			    Reply.sendCOMMAND("proveconjecture "+cmdv.get(index));
		    }
		    else
		    if (Jape.onLinux || Jape.onSolaris) {
			// workaround for JList bug?
			// probably the test ought to be on the L&F, but this might work
			int oldsel = list.getSelectedIndex();
			if (oldsel==index)
			    repaintCell(index);
			else {
			    list.setSelectedIndex(index);
			    repaintCell(oldsel); repaintCell(index);
			}
		    }
		}
	    };
	    list.addMouseListener(m);

	    scrollPane = new JScrollPane(list, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
					       JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
	    contentPane.add(scrollPane);

	    buttonPane = new ButtonPane(Jape.onMacOSX ? 4 : 3);
	    for (int i=0; i<buttonv.size(); i++) {
		PanelButton b = (PanelButton)buttonv.get(i);
		b.font_reset();
		addButton(b); // make sure it gets an ActionListener
	    }
	    contentPane.add(buttonPane);
	    
	    list.addListSelectionListener(new ButtonWatcher());
	    
	    if (LocalSettings.panelWindowMenus)
		setBar(); // by experiment, seems to be necessary before setVisible

	    setSize(getPreferredSize());
	    setLocation(nextPos());

	    enableButtons();
	    pack(); // necessary??
	}

	public int getBarKind() {
	    return JapeMenu.OTHERWINDOW_BAR;
	}
	
	protected boolean servesAsControl() { return LocalSettings.panelWindowMenus; }

	public void resetPanelLayout() {
	    Container contentPane = getContentPane();
	    contentPane.getLayout().layoutContainer(contentPane);
	}

	private void setListFont() {
	    JapeFont.setComponentFont(list, JapeFont.PANELENTRY);
	    prefixw = JapeFont.stringWidth(list, "YN ");
	}

	public void font_reset() {
	    for (int i=0; i<buttonv.size(); i++)
		((PanelButton)buttonv.get(i)).font_reset();
	    buttonPane.doLayout();
	    setListFont();
	    for (int i=0; i<model.size(); i++) {
		Object e = model.getElementAt(i);
		if (e instanceof Entry)
		    ((Entry)e).font_reset();
	    }
	    invalidate();
	    repaint();
	    if (list.getSelectedIndex()!=-1)
		list.ensureIndexIsVisible(list.getSelectedIndex());
	}
	
	public boolean equals(Object o) {
	    return o instanceof PanelWindow ? ((PanelWindow)o).title.equals(this.title) :
					      super.equals(o);
	}

	protected class Entry extends Component {
	    private String s;
	    public String prefix;
	    private TextDimension td; // changes when fonts change
	    private Dimension size;   // ditto
	    public boolean selected;
	    public boolean marked;
	    public Color innerBackground = Color.white;
	    public Entry(String s) {
		super(); this.s = s; prefix = null;
		font_reset();
	    }
	    public Dimension getPreferredSize() {
		return new Dimension (size.width, size.height);
	    }
	    public void font_reset() {
		setFont(list.getFont());
		this.td = JapeFont.measure(list, s);
		this.size = new Dimension(prefixw+td.width, td.ascent+td.descent);
	    }
	    public void paint(Graphics g) {
		if (DebugVars.paint_tracing)
		    Logger.log.println("painting PanelWindow.Entry");
		g.setColor(getBackground()); g.fillRect(0, 0, getWidth(), getHeight());
		if (selected && !active) {
		    g.setColor(innerBackground); g.fillRect(2, 2, getWidth()-4, getHeight()-4);
		}
		g.setColor(getForeground()); g.setFont(getFont());
		if (prefix!=null) g.drawString(prefix, 0, td.ascent);
		g.drawString(s, prefixw, td.ascent);
	    }
	    public void mark(boolean proved, boolean disproved) {
		marked = proved || disproved;
		prefix = proved ? LocalSettings.tick+(disproved ? LocalSettings.cross : "") :
			 disproved ? " "+LocalSettings.cross :
			 null;
	    }
	}
	
	protected void addEntry(String entry, Mark mark) {
	    Entry e = new Entry(entry);
	    model.addElement(e);
	    if (mark!=null)
		e.mark(mark.proved, mark.disproved);
	}

	protected void markEntry(int i, boolean proved, boolean disproved) {
	    Entry e = (Entry)model.elementAt(i);
	    e.mark(proved, disproved);
	    repaintCell(i);
	    enableButtons();
	}

	protected void selectEntry(int i) {
	    list.setSelectedIndex(i);
	    list.ensureIndexIsVisible(i);
	}

	private void repaintSelection() {
	    repaintCell(list.getSelectedIndex());
	}

	private void repaintCell(int index) {
	    if (0<=index && index<model.size()) {
		Point p = list.indexToLocation(index);
		Dimension d = ((Entry)model.elementAt(index)).getPreferredSize();
		list.repaint(p.x, p.y, d.width, d.height);
	    }
	}

	class Renderer implements ListCellRenderer {
	    public Component getListCellRendererComponent(JList list, Object value, int index,
							  boolean isSelected, boolean cellHasFocus) {
		Entry e = (Entry)value;
		if (isSelected) {
		    e.setBackground(list.getSelectionBackground());
		    e.setForeground(list.getSelectionForeground());
		}
		else {
		    e.setBackground(list.getBackground());
		    e.setForeground(list.getForeground());
		}
		e.selected = isSelected; 
		if (isSelected && !cellHasFocus)
		    e.innerBackground = list.getBackground();
		return e;
	    }
	}
	
	protected void addButton(PanelButton button) {
	    buttonPane.addButton(button);
	    button.addActionListener(this);
	}

	protected void removeButton(PanelButton button) {
	    button.removeActionListener(this);
	    buttonPane.removeButton(button);
	}
	
	// ActionListener interface for buttons
    
	public void actionPerformed(ActionEvent newEvent) {
	    String key = newEvent.getActionCommand();
	    for (int i=0; i<buttonv.size(); i++)  {
		PanelButton b = (PanelButton)buttonv.get(i);
		if (b.label.equals(key)) {
		    b.doAction(this.title, list.getSelectedIndex());
		    return;
		}
	    }
	    Alert.abort("PanelWindow.actionPerformed: no button "+JapeUtils.enQuote(key)+
			" in panel "+this.title);
	}

	protected class ButtonWatcher implements ListSelectionListener {
	    public void valueChanged(ListSelectionEvent e) {
		enableButtons();
	    }
	}

	protected void enableButtons() {
	    int index = list.getSelectedIndex();
	    if (0<=index && index<model.size()) {
		for (int i=0; i<buttonv.size(); i++) {
		    PanelButton b = (PanelButton)buttonv.get(i);
		    if (kind==ConjecturePanelKind && b.label.equals(showproofLabel))
			b.setEnabled(((Entry)model.get(index)).marked);
		    else
			b.setEnabled(true);
		}
	    }
	    else {
		for (int i=0; i<buttonv.size(); i++) {
		    PanelButton b = (PanelButton)buttonv.get(i);
		    b.setEnabled(!b.needsSelection());
		}
	    }
	}

	protected void enableButton(String label, boolean state) {
	    PanelButton b = findButton(label);
	    if (b!=null)
		b.setEnabled(state);
	}

	// it seems that this method is never called, even when the window is maximised.
	
	public Dimension getMaximumSize() {
	    if (panellayout_tracing)
		Logger.log.println("panel "+this.title+" getMaximumSize called");
	    return super.getMaximumSize();
	}

	// the Container class seems to cache this, but I think it needs calling more than once ...
	public Dimension getPreferredSize() {
	    if (panellayout_tracing)
		Logger.log.println("panel "+this.title+" getPreferredSize called");
	    return super.getPreferredSize();
	}
	
	/**********************************************************************************************
    
	    Layout
    
	**********************************************************************************************/
    
	protected class PanelWindowLayout implements LayoutManager {
    
	    /* Called by the Container add methods. Layout managers that don't associate
	     * strings with their components generally do nothing in this method.
	     */
	    public void addLayoutComponent(String s, Component c) { }
    
	    /* Adds the specified component to the layout, using the specified constraint object. */
	    public void addLayoutComponent(Component comp, Object constraints) { }
    
	    /* Invalidates the layout, indicating that if the layout manager has cached information
	     * it should be discarded.
	     */
	    public void invalidateLayout(Container pane) { } // we don't cache
    
	    /* Returns the maximum size of this component.
	     * Never called, so far as I could tell, when this was a LayoutManager2,
	     * but preserved in case it might one day be useful
	    public Dimension maximumLayoutSize(Container pane) {
		// crude, for now
		JViewport port = scrollPane.getViewport();
		int width = port.getX()+list.getWidth(), height = port.getY()+list.getHeight();
		if (buttonv.size()!=0) {
		    Dimension d = ((PanelButton)buttonv.get(0)).getPreferredSize();
		    int buttonleading = leading(d);
		    height += d.height+2*buttonleading;
		    int buttonwidth = (buttonv.size()-1)*buttonleading;
		    for (int i=0; i<buttonv.size(); i++)
			buttonwidth+=((PanelButton)buttonv.get(i)).getPreferredSize().width;
		    width = Math.max(width, buttonwidth);
		}
		if (panellayout_tracing)
		    Logger.log.println("maximumLayoutSize returns "+width+","+height);
		return new Dimension(width, height);
	    }
	    */
		    
	    /* Called by the Container remove and removeAll methods. Many layout managers
	     * do nothing in this method, relying instead on querying the container for its
	     * components, using the Container getComponents method.
	     */
	    public void removeLayoutComponent(Component c) { }
    
	    /* Called by the Container getPreferredSize method, which is itself called under
	     * a variety of circumstances. This method should calculate and return the ideal
	     * size of the container, assuming that the components it contains will be at or
	     * above their preferred sizes. This method must take into account the container's
	     * internal borders, which are returned by the getInsets method.
	     */
    
	    public Dimension preferredLayoutSize(Container pane) {
		JViewport port = scrollPane.getViewport();
		Dimension d = buttonPane.getPreferredSize();
		Dimension preferredSize =
		    new Dimension(Math.max(d.width, Math.min(d.width*4/3, port.getX()+list.getWidth())),
				  d.width+d.height*2/3);
		return preferredSize;
	    }
    
	    /* Called by the Container getMinimumSize method, which is itself called under
	     * a variety of circumstances. This method should calculate and return the minimum
	     * size of the container, assuming that the components it contains will be at or
	     * above their minimum sizes. This method must take into account the container's
	     * internal borders, which are returned by the getInsets method.
	     */
	    
	    public Dimension minimumLayoutSize(Container pane) {
		Dimension d = buttonPane.getPreferredSize();
		d.height += d.width;
		return d; 
	    }
    
	    /* Called when the container is first displayed, and each time its size changes.
	     * A layout manager's layoutContainer method doesn't actually draw components.
	     * It simply invokes each component's resize, move, and reshape methods to set
	     * the component's size and position. This method must take into account the
	     * container's internal borders, which are returned by the getInsets method.
	     * You can't assume that the preferredLayoutSize or minimumLayoutSize method
	     * will be called before layoutContainer is called.
	     */
	    public void layoutContainer(Container pane) {
		Dimension buttonPaneSize = buttonPane.doLayout(pane.getWidth());
		if (buttonPaneSize.height==0) {
		    buttonPane.setBounds(0, 0, 0, 0);
		    scrollPane.setBounds(pane.getBounds());
		}
		else {
		    int buttonTop = pane.getHeight()-buttonPaneSize.height;
		    buttonPane.setBounds(0, buttonTop, pane.getWidth(), buttonPaneSize.height); 
		    scrollPane.setBounds(0, 0, pane.getWidth(), Math.max(0, buttonTop));
		    if (panellayout_tracing) {
			Logger.log.print("panelwindow "+PanelWindow.this.title+" layout: ");
			JapeUtils.showContainer(pane);
		    }
		}
	    }
	}
    }

    private void initWindow() {
	window = new PanelWindow();
    }

    /**********************************************************************************************

	Static interface for Dispatcher

     **********************************************************************************************/

    private static Vector panelv = new Vector();
    
    public static PanelWindowData spawn (String title, int kind) throws ProtocolError {
	PanelWindowData p = findPanel(title);
	if (p!=null) {
	    if (p.kind==kind)
		return p;
	    else
		throw new ProtocolError ("panel already exists (with different kind)");
	}
	else {
	    // it ain't there
	    PanelWindowData panel = new PanelWindowData(title, kind);
	    panelv.add(panel);
	    return panel;
	}
    }

    protected static PanelWindowData findPanel(String title) {
	for (int i=0; i<panelv.size(); i++) {
	    PanelWindowData panel = (PanelWindowData)panelv.get(i);
	    if (panel.title.equals(title))
		return panel;
	}
	return null;
    }

    protected static PanelWindowData mustFindPanel(String title) throws ProtocolError {
	PanelWindowData panel = findPanel(title);
	if (panel==null)
	    throw new ProtocolError("no such panel");
	else
	    return panel;
    }

    public static void makePanelsVisible() {
	for (int i=0; i<panelv.size(); i++) {
	    PanelWindowData panel = (PanelWindowData)panelv.get(i);
	    if (panel.window==null)
		panel.initWindow();
	    else {
		panel.window.resetPanelLayout();
		// but don't reset the size or position ...
	    }
	    panel.window.setVisible(true);
	}
    }

    public static void font_reset() {
	for (int i=0; i<panelv.size(); i++) {
	    PanelWindowData panel = (PanelWindowData)panelv.get(i);
	    if (panel.window!=null)
		panel.window.font_reset();
	}
    }

    public static void cancelPanels() {
	while (panelv.size()!=0) {
	    ((PanelWindowData)panelv.get(0)).closeWindow();
	    panelv.remove(0);
	}
    }

    public static void emptyPanels() {
	for (int i=0; i<panelv.size(); i++)
	    ((PanelWindowData)panelv.get(i)).emptyPanel();
    }

    public static void addEntry(String panel, String entry, String cmd) throws ProtocolError {
	mustFindPanel(panel).addEntry(entry,cmd);
    }
    
    public static void addButton(String panel, String button, Insert[] cmd) throws ProtocolError {
	mustFindPanel(panel).addButton(button, cmd, false);
    }

    public static void markEntry(String panel, String cmd /* really */,
				 boolean proved, boolean disproved) throws ProtocolError {
	mustFindPanel(panel).markEntry(cmd, proved, disproved);
    }

    public static void selectEntry(String panel, String entry) throws ProtocolError {
	mustFindPanel(panel).selectEntry(entry);
    }
}
