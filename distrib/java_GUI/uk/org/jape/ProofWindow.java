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

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;

import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;


import java.awt.geom.AffineTransform;

import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;

import java.util.Enumeration;
import java.util.Vector;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuBar;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.SwingUtilities;

public class ProofWindow extends JapeWindow implements DebugConstants, ProtocolConstants,
						       SelectionConstants,
						       Printable {
    public final int proofnum;

    protected AnchoredScrollPane proofPane;
    protected ProofCanvas proofCanvas;
    protected DisproofPane disproofPane; // more complicated than the others
    protected AnchoredScrollPane provisoPane;
    protected ProvisoCanvas provisoCanvas;
    protected JSplitPane mainSplitPane, subSplitPane;
    
    protected JapeCanvas focussedCanvas;

    protected WindowListener windowListener;
    
    public ProofWindow(final String title, final int proofnum) {
	super(title, proofnum);
	this.proofnum = proofnum;
	
	getContentPane().setLayout(new BorderLayout()); 
	proofPane = new AnchoredScrollPane();
	proofCanvas = new ProofCanvas(proofPane.getViewport(), true);
	proofPane.add(proofCanvas);
	
	getContentPane().add(proofPane, BorderLayout.CENTER);

	setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);

	focusManager.insertInfocusv(this);

	windowListener = new WindowAdapter() {
	    public void windowClosing(WindowEvent e) {
		if (windowListener!=null)
		    Reply.sendCOMMAND("closeproof "+proofnum);
		else
		    Logger.log.println("ProofWindow.windowListener late windowClosing \""+
				       title +"\"; "+e);
	    }
	    public void windowActivated(WindowEvent e) {
		if (windowListener!=null) {
		    if (focusManager.setTopInfocusv(ProofWindow.this)) {
			focusManager.reportFocus();
			doEnableMenuItems();
		    }
		}
		else
		    Logger.log.println("ProofWindow.windowListener late windowActivated \""+
				       title +"\"; "+e);
	    }
	};
	addWindowListener(windowListener);

	setBar();
	doEnableMenuItems();

	pack();
	setSize(LocalSettings.DefaultProofWindowSize);
	setLocation(nextPos());
	setVisible(true);
    }

    protected boolean servesAsControl() { return true; }

    public boolean equals(Object o) {
	return o instanceof ProofWindow ? ((ProofWindow)o).title.equals(title) &&
					       ((ProofWindow)o).proofnum==proofnum :
					  super.equals(o);
    }

    private boolean isProofFocus() {
	return disproofPane==null || focussedPane==proofPane;
    }
    
    public String undoSuffix() {
	if (isProofFocus())
	    return "proof";
	else
	    return "disproof";
    }

    public void closeProof() {
	Reply.sendCOMMAND("closeproof "+proofnum);
    }

    public void enableCopy() {
	int proofcount = proofCanvas.getTextSelectionCount(),
	    disproofcount = disproofPane==null ? 0 : disproofPane.getTextSelectionCount(),
	    provisocount = provisoCanvas==null ? 0 : provisoCanvas.getTextSelectionCount();

	try {
	    JapeMenu.enableItem(true, "Edit", "Copy", proofcount+disproofcount+provisocount==1);
	} catch (ProtocolError e) {
	    Alert.abort("ProofWindow.enableCopy can't find Edit: Copy");
	}
	
	/*  try {
		JapeMenu.enableItem(true, "Edit", "Copy Ascii",
				    proofcount+disproofcount+provisocount==1);
	    } catch (ProtocolError e) {
		Alert.abort("ProofWindow.enableCopy can't find Edit: Copy Ascii");
	    }
	 */
    }

    public String getSingleTextSelection() {
	int proofcount = proofCanvas.getTextSelectionCount(),
	    disproofcount = disproofPane==null ? 0 : disproofPane.getTextSelectionCount(),
	    provisocount = provisoCanvas==null ? 0 : provisoCanvas.getTextSelectionCount();

	return proofcount+disproofcount+provisocount!=1 ? null :
	       proofcount==1	? proofCanvas.getSingleTextSelection() :
	       disproofcount==1 ? disproofPane.getSingleTextSelection() :
				  provisoCanvas.getSingleTextSelection();
    }

    public void enableUndo() {
	boolean undoenable, redoenable;
	
	if (isProofFocus()) {
	    undoenable = proofhistory; redoenable = prooffuture;
	}
	else {
	    undoenable = disproofhistory; redoenable = disprooffuture;
	}

	try {
	    JapeMenu.enableItem(true, "Edit", "Undo", undoenable);
	    JapeMenu.enableItem(true, "Edit", "Redo", redoenable);
	} catch (ProtocolError e) {
	    Alert.abort("ProofWindow.enableUndo can't find Edit: Undo/Redo");
	}

	if (Jape.onMacOS) // put the dot in the red button
	    getRootPane(). putClientProperty("windowModified",
			    (proofhistory||disproofhistory) ? Boolean.TRUE : Boolean.FALSE);
    }

    public void enableExport() {
	try {
	    JapeMenu.enableItem(true, "File", JapeMenu.EXPORT_PROOF, disproofPane!=null);
	    JapeMenu.enableItem(true, "File", JapeMenu.EXPORT_DISPROOF, disproofPane!=null);
	} catch (ProtocolError e) {
	    Alert.abort("ProofWindow.enableExport can't find File: Export Proof/Disproof");
	}
    }
    
    // pane focus
    
    private Container focussedPane = null;

    protected void claimProofFocus() {
	focussedPane = proofPane;
	enableUndo();
    }

    protected void claimDisproofFocus() {
	focussedPane = disproofPane;
	enableUndo();
    }
    
    /**********************************************************************************************

	Printing

     **********************************************************************************************/

    /* Print layout (with obvious modifications if disproof and/or provisos are empty):

	 ----------------------	 
	|		       |
	|      disproof	       |
	|		       |
	 ---------------------- 
	(line)
	 ---------------------------
	|			    |
	|			    |
	|      proof		    |
	|			    |
	|			    |
	 ---------------------------
	(line)
	 ---------------------
	|		      |
	|    provisos	      |
	|		      |
	 ---------------------
     */
    
    private int gap() { return 5*proofCanvas.linethickness; }

    private int separatorThickness() { return 2*proofCanvas.linethickness; }

    public int whattoprint;
    
    public class PrintSize {
	public final int printHeight, printWidth,
			 disproofHeight, disproofWidth,
			 disproofSepY, disproofSepWidth,
			 proofY,
			 provisoY, provisoSepY, provisoSepWidth;
	
	public PrintSize() {
	    int h=0, w=0, proofW = proofCanvas.getWidth(),
		sepH = separatorThickness(), gap = gap();

	    if ((whattoprint & PrintProof.DISPROOF)!=0 && disproofPane!=null) {
		Dimension disproofSize = disproofPane.printSize();
		w = disproofWidth = disproofSize.width;
		disproofHeight = disproofSize.height;
		h = disproofHeight;
	    } else {
		this.disproofHeight = this.disproofWidth = 0;
	    }

	    if ((whattoprint & PrintProof.PROOF)!=0) {
		if (h!=0) {
		    h+=gap+sepH+gap;
		    disproofSepY = disproofHeight+gap;
		    disproofSepWidth = Math.max(disproofWidth, proofW);
		    proofY = disproofSepY+sepH+gap;
		} else {
		    this.disproofSepY = this.disproofSepWidth = this.proofY = 0;
		}
		w = Math.max(w, proofW);
		h += proofCanvas.getHeight();

		if (provisoCanvas!=null) {
		    int provisoW = provisoCanvas.getWidth();
		    provisoSepY = h+gap; provisoSepWidth = Math.max(proofW, provisoW);
		    h += gap+sepH+gap;
		    provisoY = h; h+=provisoCanvas.getHeight();
		    w = Math.max(w, provisoW);
		}
		else {
		    provisoSepY = provisoSepWidth = provisoY = 0;
		}
	    } else {
		disproofSepY = disproofSepWidth = proofY =
		provisoSepY = provisoSepWidth = provisoY = 0;
	    }

	    this.printHeight = h; this.printWidth = w;
	}
    }

    public PrintSize getPrintSize() {
	return new PrintSize();
    }

    private void printSeparator(Graphics2D g2D, int y, int length) {
	g2D.setColor(Preferences.SeparatorColour);
	g2D.setStroke(new BasicStroke((float)separatorThickness()));
	g2D.drawLine(0, y, length, y);
    }

    /* By experiment, this method seems to be called three times by any printing operation ...
       don't know what that does for error messages.
     */
    public int print(Graphics g, final PageFormat pf, int pi) throws PrinterException {
	
	if (pi!=0) {
	    final int pix=pi;
	    if (DebugVars.printdialog_tracing)
		SwingUtilities.invokeLater(
		    new Runnable() {
			public void run() {
			    Logger.log.println("ProofWindow.print pi="+pix);
			}
		    });
	    return Printable.NO_SUCH_PAGE;
	}

	if (!(g instanceof Graphics2D)) {
	    SwingUtilities.invokeLater(
		new Runnable() {
		    public void run() {
			Logger.log.println("ProofWindow.print can't do Graphics2D");
			Alert.showAlert(Alert.Warning,
					"Can't print: this seems to be a very old version of Java");
		    }
		});
	    return Printable.NO_SUCH_PAGE;
	}

	Graphics2D g2D = (Graphics2D) g;
	
	if (printlayout_tracing) {
	    Logger.log.println("ProofWindow.print("+g2D+")");
	    JapeUtils.showContainer(proofCanvas);
	}
	
	g2D.translate((int)pf.getImageableX(), (int)pf.getImageableY());

	// scale if necessary 
	final PrintSize printSize = new PrintSize();
	double scalex = (double)pf.getImageableWidth()/(double)printSize.printWidth,
	       scaley = (double)pf.getImageableHeight()/(double)printSize.printHeight;
	final double scale = Math.min(scalex, scaley);
	AffineTransform trans = g2D.getTransform();
	
	if (scale<1.0) {
	    SwingUtilities.invokeLater(
		new Runnable() {
		    public void run() {
			Alert.showAlert("scaling printing to "+scale);
		    }
		});
	    g2D.scale(scale, scale);
	}

	if ((whattoprint & PrintProof.DISPROOF)!=0 && disproofPane!=null) {
	    disproofPane.print(g);
	    if (printSize.disproofSepWidth!=0)
		printSeparator(g2D, printSize.disproofSepY, printSize.disproofSepWidth);
	}

	if ((whattoprint & PrintProof.PROOF)!=0) {
	    g2D.translate(0, printSize.proofY);
	    proofCanvas.paint(g);
	    g2D.translate(0, -printSize.proofY);

	    if (provisoCanvas!=null) {
		printSeparator(g2D, printSize.provisoSepY, printSize.provisoSepWidth);
		g2D.translate(0, printSize.provisoY);
		provisoCanvas.paint(g);
		g2D.translate(0, -printSize.provisoY);
	    }
	}
	
	g2D.setTransform(trans);
	
	g2D.translate(-((int)pf.getImageableX()), -((int)pf.getImageableY()));
	
	return Printable.PAGE_EXISTS;
    }

    private void initProofCanvas(byte style, int linethickness) {
	switch(style) {
	    case BoxStyle:
		proofPane.setAnchor(AnchoredScrollPane.ANCHOR_SOUTHWEST); break;
	    case TreeStyle:
		proofPane.setAnchor(AnchoredScrollPane.ANCHOR_SOUTH); break;
	    default:
		Alert.abort("ProofWindow.initProofCanvas style="+style);
	}
	proofPane.validate(); proofPane.repaint();
	focussedCanvas = proofCanvas; // really?
	proofCanvas.proofStyle = style;
	proofCanvas.setlinethickness(linethickness);
    }

    private DisproofPane ensureDisproofPane() {
	if (disproofPane==null) {
	    disproofPane = new DisproofPane(this, proofCanvas.linethickness);
	    enableExport();
	    disproofPanePending = true;
	    claimProofFocus(); // because nothing happened yet
	}
	return disproofPane;
    }

    private AnchoredScrollPane ensureProvisoPane() {
	if (provisoPane==null) {
	    provisoPane = new AnchoredScrollPane();
	    provisoCanvas = new ProvisoCanvas(provisoPane.getViewport(), true);
	    provisoPane.add(provisoCanvas);
	    provisoPane.setAnchor(AnchoredScrollPane.ANCHOR_NORTHWEST);
	    provisoPanePending = true;
	}
	return provisoPane;
    }

    private boolean disproofPanePending = false,
	provisoPanePending  = false;

    public void makeWindowReady() {
	if(disproofPanePending) {
	    disproofPanePending = false;
	    Component other = subSplitPane==null ? (Component)proofPane : (Component)subSplitPane;
	    Dimension paneSize = other.getSize();
	    getContentPane().remove(proofPane);
	    mainSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, disproofPane, other);
	    mainSplitPane.setResizeWeight(0.5);
	    mainSplitPane.setPreferredSize(paneSize);
	    mainSplitPane.setSize(paneSize);
	    getContentPane().add(mainSplitPane, BorderLayout.CENTER);
	    mainSplitPane.validate();
	    mainSplitPane.repaint();
	}
	if (provisoPanePending) {
	    provisoPanePending = false;
	    Dimension paneSize = proofPane.getSize();
	    subSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true);
	    if (mainSplitPane==null) {
		getContentPane().remove(proofPane);
		getContentPane().add(subSplitPane, BorderLayout.CENTER);
	    }
	    else {
		mainSplitPane.setRightComponent(subSplitPane);
	    }
	    subSplitPane.setLeftComponent(proofPane);
	    subSplitPane.setRightComponent(provisoPane);
	    subSplitPane.setResizeWeight(1.0);
	    subSplitPane.setPreferredSize(paneSize);
	    subSplitPane.setSize(paneSize);
	    subSplitPane.validate();
	    subSplitPane.repaint();
	}

	if (disproofPane!=null)
	    disproofPane.makeReady();
    }

    private JapeCanvas byte2JapeCanvas(byte pane) throws ProtocolError {
	switch (pane) {
	    case ProofPaneNum:
		return proofCanvas;
	    case DisproofPaneNum:
		return ensureDisproofPane().seqCanvas; // really? really!
	    default:
		throw new ProtocolError("invalid pane number");
	}
    }

    public Rectangle getPaneGeometry(byte pane) throws ProtocolError {
	return byte2JapeCanvas(pane).getViewGeometry();
    }

    public void clearPane(byte pane) throws ProtocolError {
	// don't create a disproof pane just to clear it ...
	if (pane==DisproofPaneNum && disproofPane==null)
	    return;
	else
	    byte2JapeCanvas(pane).removeAll();
    }
    
    public void setProofParams(byte style, int linethickness) throws ProtocolError {
	initProofCanvas(style, linethickness);
	if (disproofPane!=null)
	    disproofPane.setlinethickness(linethickness);
    }

    public void drawInPane(byte pane) throws ProtocolError {
	switch (pane) {
	    case ProofPaneNum:
		focussedCanvas = proofCanvas;
		break;
	    case DisproofPaneNum:
		focussedCanvas = ensureDisproofPane().seqCanvas;
		break;
	    default:
		throw new ProtocolError("invalid pane number");
	}
    }

    private int mtX, mtY, mtCount;
    private byte mtKind;
    private TextComponent[] mtLines;
    
    public void startMeasuredText(int x, int y, byte kind, int length) {
	mtX = x; mtY = y; mtKind = kind;
	mtCount = 0;
	mtLines = kind==PunctTextItem ? new TextComponent[length] : 
	                                new AnnotatedTextComponent[length];
    }
    
    public void continueMeasuredText(int x, int y, byte fontnum, 
				     String text) throws ProtocolError {
	JapeFont.checkInterfaceFontnum(fontnum);
	mtLines[mtCount++] = 
	    mtKind==PunctTextItem ? new TextComponent(x, y, fontnum, text)
				  : new AnnotatedTextComponent(x, y, fontnum, text);
    }
    
    public void endMeasuredText() throws ProtocolError {
	if (mtCount!=mtLines.length)
	    Alert.showErrorAlert("measured text error: we got "+mtCount+" lines; we expected "+
				 mtLines.length);
	drawstring(mtX, mtY, mtKind, mtLines);
    }
    
    public void drawstring(int x, int y, byte fontnum, byte kind,
			   String text) throws ProtocolError {
	JapeFont.checkInterfaceFontnum(fontnum);
	drawstring(x, y, kind, 
		   kind==PunctTextItem ? 
		    new TextComponent[]{new TextComponent(0, 0, fontnum, text)} : 
		    new AnnotatedTextComponent[]{
			new AnnotatedTextComponent(0, 0, fontnum, text)});
   }
    
    public void drawstring(int x, int y, byte kind, 
			   TextComponent[] components) throws ProtocolError {
	byte selectionKind;
	boolean ambiguous = false;
	
	switch (kind) {
	    case PunctTextItem:
		focussedCanvas.add(new TextItem(focussedCanvas, x, y, components));
		return;
		
	    case HypTextItem:
		selectionKind = HypSel;
		break;
		
	    case ConcTextItem:
		selectionKind = ConcSel;
		break;
		
	    case AmbigTextItem:
		selectionKind = ConcSel; // doesn't matter
		ambiguous = true;
		break;
		
	    case ReasonTextItem:
		selectionKind = ReasonSel;
		break;
		
	    default:
		throw new ProtocolError("invalid item kind");
	}
	
	// fall through to determine which canvas to add to
	if (focussedCanvas instanceof ProofCanvas)
	    focussedCanvas.add(
	       new SelectableProofItem((ProofCanvas)focussedCanvas, x, y, selectionKind, 
				       ambiguous, (AnnotatedTextComponent[])components));
	else
	if (focussedCanvas instanceof DisproofCanvas)
	    focussedCanvas.add(
	       new EmphasisableItem((DisproofCanvas)focussedCanvas, x, y, 
				    (AnnotatedTextComponent[])components));
	else
	    throw new ProtocolError("drawstring in "+focussedCanvas);
    }
	
	
    public void drawRect(int x, int y, int w, int h) {
	proofCanvas.add(new RectItem(proofCanvas, x, y, w, h));
    }

    public void drawLine(int x1, int y1, int x2, int y2) {
	proofCanvas.add(new LineItem(proofCanvas, x1, y1, x2, y2));
    }

    public String getProofSelections() {
	String s = proofCanvas.getSelections("\n");
	return s==null ? "" : s+"\n";
    }

    public String getProofTextSelections() throws ProtocolError {
	String s = proofCanvas.getPositionedContextualisedTextSelections("\n");
	return s==null ? "" : s+"\n";
    }

    public String getDisproofSelections() {
	String s = disproofPane==null ? null : disproofPane.seqCanvas.getSelections("\n");
	return s==null ? "" : s+"\n";
    }

    public String getDisproofTextSelections() throws ProtocolError {
	String s = disproofPane==null ? null :
			disproofPane.seqCanvas.getPositionedContextualisedTextSelections("\n");
	return s==null ? "" : s+"\n";
    }

    public String getGivenTextSelections() throws ProtocolError {
	String s = provisoPane==null ? null : provisoCanvas.getTextSelections(Reply.stringSep);
	return s==null ? "" : s; 
    }
    
    // disproof stuff
    public void setSequentBox(int w, int a, int d) throws ProtocolError {
	ensureDisproofPane().setSequentBox(w, a, d);
    }

    public void setDisproofTiles(String[] tiles) throws ProtocolError {
	ensureDisproofPane().setTiles(tiles);
    }

    public void worldsStart() throws ProtocolError {
	ensureDisproofPane().worldCanvas.worldsStart();
    }

    public void addWorld(int x, int y, boolean forcedhere) throws ProtocolError {
	ensureDisproofPane().worldCanvas.addWorld(x, y, forcedhere);
    }

    public void addWorldLabel(int x, int y, boolean forced, String label) throws ProtocolError {
	ensureDisproofPane().worldCanvas.addWorldLabel(x, y, forced, label);
    }

    public void addChildWorld(int x, int y, int xc, int yc) throws ProtocolError {
	ensureDisproofPane().worldCanvas.addChildWorld(x, y, xc, yc);
    }

    public void selectWorld(int x, int y, boolean selected) throws ProtocolError {
       ensureDisproofPane().worldCanvas.selectWorld(x, y, selected);
    }

    private EmphasisableItem findDisproofEmphasisableXY(int x, int y) throws ProtocolError {
	EmphasisableItem ei;
	if (disproofPane!=null && (ei = disproofPane.seqCanvas.findEmphasisable(x,y))!=null)
	    return ei;
	else
	    throw new ProtocolError("no such item");
    }

    public void emphasise(int x, int y, boolean state) throws ProtocolError {
	findDisproofEmphasisableXY(x,y).emphasise(state);
    }

    public void forceDisproofSelect(int x, int y) throws ProtocolError {
	findDisproofEmphasisableXY(x,y).setSelected(true);
    }

    public void forceDisproofTextSelection(int x, int y, String[] ss) throws ProtocolError {
	findDisproofEmphasisableXY(x,y).setTextSels(ss);
    }

    // provisos and givens

    public void clearProvisoView() throws ProtocolError {
	// don't create a provisoPane just to clear it
	if (provisoCanvas!=null) {
	    provisoCanvas.clear();
	}
    }

    public void showProvisoLine(String annottext) throws ProtocolError {
	ensureProvisoPane();
	provisoCanvas.addProvisoLine(annottext);
    }

    public void setGivens(MiscellaneousConstants.IntString[] gs) throws ProtocolError {
	// don't create a provisoPane just to say there aren't any givens
	if (provisoPane!=null || (gs!=null && gs.length!=0)) {
	    ensureProvisoPane();
	    provisoCanvas.setGivens(gs);
	}
    }

    private void checkFocussedCanvas() throws ProtocolError {
	if (focussedCanvas==null)
	    throw new ProtocolError("no focussed pane - drawInPane missing?");
    }

    private SelectableProofItem findProofSelectableXY(int x, int y) throws ProtocolError {
	SelectableProofItem si = proofCanvas.findSelectable(x,y);
	if (si==null)
	    throw new ProtocolError("no selectable proof item at "+x+","+y);
	else
	    return si;
    }

    public void blacken(int x, int y) throws ProtocolError {
	findProofSelectableXY(x,y).blacken();
    }

    public void greyen(int x, int y) throws ProtocolError {
	findProofSelectableXY(x,y).greyen();
    }

    public void highlight(int x, int y, byte selclass) throws ProtocolError {
	byte selkind;
	switch (selclass) {
	    case ConcTextItem  : selkind = ConcSel;   break;
	    case HypTextItem   : selkind = HypSel;    break;
	    case ReasonTextItem: selkind = ReasonSel; break;
	    default	       :
		throw new ProtocolError("selclass not ConcTextItem/HypTextItem/ReasonTextItem");
	}
	SelectableProofItem i = findProofSelectableXY(x,y);
	i.setSelected(true);
	i.setSelectionKind(selkind);
    }

    public void unhighlight(int x, int y) throws ProtocolError {
	findProofSelectableXY(x,y).setSelected(false);
    }

    // synchronized access only to the focus vector

    private static FocusManager focusManager = new FocusManager();

    private static class FocusManager {
	private Vector focusv = new Vector();

	public synchronized ProofWindow maybeFocussedWindow() {
	    if (focusv.size()==0)
		return null;
	    else
		return (ProofWindow)focusv.get(0);
	}

	private synchronized void insertInfocusv(ProofWindow w) {
	    focusv.insertElementAt(w, 0);
	}

	private synchronized boolean setTopInfocusv(ProofWindow w) {
	    int i = focusv.indexOf(w);
	    if (i==-1)
		Alert.abort("unfocussable proof "+w.title);
	    else
		if (i!=0) {
		    focusv.remove(i);
		    focusv.insertElementAt(w,0);
		}
	    return i!=0;
	}

	private synchronized void removeFromfocusv(ProofWindow w) {
	    int i = focusv.indexOf(w);
	    if (i==-1)
		Alert.abort("unremovable proof "+w.title);
	    else
		focusv.remove(i);
	}

	public synchronized void reportFocus() {
	    if (focusv.size()!=0)
		Reply.sendCOMMAND("setfocus "+((ProofWindow)focusv.get(0)).proofnum);
	}

	public synchronized void makeReady() {
	    for (int i = 0; i<focusv.size(); i++)
		((ProofWindow)focusv.get(i)).makeWindowReady();
	}
    }

    /**********************************************************************************************

	Static interface for Dispatcher

     **********************************************************************************************/

    public static ProofWindow spawn(String title, int proofnum) throws ProtocolError {
	if (JapeWindow.findWindow(title)!=null)
	    throw new ProtocolError("already a window with that title");
	else {
	    final ProofWindow w = new ProofWindow(title, proofnum);
	    return w;
	}
    }

    private static ProofWindow findProof(int proofnum) throws ProtocolError {
	ProofWindow w = windowList.findProofWindow(proofnum);
	if (w==null)
	    throw new ProtocolError("no proof numbered "+proofnum);
	return w;
    }

    public static void closeproof(int proofnum) throws ProtocolError {
	ProofWindow proof = findProof(proofnum);
	proof.removeWindowListener(proof.windowListener); // Linux gives us spurious events otherwise
	proof.windowListener = null;
	proof.closeWindow();
	focusManager.removeFromfocusv(proof);
	focusManager.reportFocus();
	enableProofMenuItems();
    }

    private static void enableProofMenuItems() {
	ProofWindow w = maybeFocussedWindow();
	if (w!=null)
	    w.doEnableMenuItems();
    }

    public void doEnableMenuItems() {
	enableCopy();
	enableUndo();
	enableExport();
    }
    
    public static ProofWindow maybeFocussedWindow() {
	return focusManager.maybeFocussedWindow();
    }

    public static ProofWindow getFocussedWindow() throws ProtocolError {
	ProofWindow w = maybeFocussedWindow();
	if (w==null)
	    throw new ProtocolError("no proof windows available");
	else
	    return w;
    }

    public static void makeReady() {
	focusManager.makeReady();
    }

    /* this doesn't work any more: has to be in a more global place */
    public static byte textselectionmode;

    public static void setTextSelectionMode(byte mode) throws ProtocolError {
	switch (mode) {
	    case SubformulaSelectionMode:
	    case TokenSelectionMode:
		textselectionmode = mode;
		break;
	    default:
		throw new ProtocolError("ProofWindow.setTextSelectionMode mode="+mode);
	}
    }

    /* because the engine thinks there is only one menu, we have to do clever things
	with undo and redo
	*/

    private static boolean proofhistory, prooffuture, disproofhistory, disprooffuture;

    public static void setHistoryVar(boolean isUndo, boolean isProof, boolean enable) {
	if (isUndo) {
	    if (isProof) proofhistory = enable;
	    else	 disproofhistory = enable;
	}
	else {
	    if (isProof) prooffuture = enable;
	    else	 disprooffuture = enable;
	}

	enableProofMenuItems();
    }
}
