/* 
    $Id$

    Copyright © 2002 Richard Bornat & Bernard Sufrin
     
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

import java.util.Enumeration;
import java.util.Vector;

import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import java.awt.geom.AffineTransform;

import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuBar;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;

public class ProofWindow extends JapeWindow implements DebugConstants, SelectionConstants,
                                                       ProtocolConstants,
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
    
    public ProofWindow(final String title, int proofnum) {
        super(title, proofnum);
        this.proofnum = proofnum;

        getContentPane().setLayout(new BorderLayout()); 
        proofPane = new AnchoredScrollPane();
        proofCanvas = new ProofCanvas(proofPane.getViewport(), true);
        proofPane.add(proofCanvas);
        
        getContentPane().add(proofPane, BorderLayout.CENTER);

        insertInfocusv();
        
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        
        windowListener = new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                if (windowListener!=null)
                    Reply.sendCOMMAND("closeproof "+ProofWindow.this.proofnum);
                else
                    System.err.println("ProofWindow.windowListener late windowClosing \""+
                                       title +"\"; "+e);
            }
            public void windowActivated(WindowEvent e) {
                if (windowListener!=null) {
                    setTopInfocusv();
                    reportFocus();
                    enableCopy();
                    enableUndo();
                }
                else
                    System.err.println("ProofWindow.windowListener late windowActivated \""+
                                       title +"\"; "+e);
            }
        };
        addWindowListener(windowListener);

        setBar();
        enableCopy(); enableUndo();

        pack();
        setSize(LocalSettings.DefaultProofWindowSize);
        setLocation(nextPos());
        setVisible(true);
    }

    private static Vector focusv = new Vector();

    private static void reportFocus() {
        if (focusv.size()!=0)
            Reply.sendCOMMAND("setfocus "+((ProofWindow)focusv.get(0)).proofnum);
    }

    private synchronized void insertInfocusv() {
        focusv.insertElementAt(this, 0);
    }

    private synchronized void setTopInfocusv() {
        int i = focusv.indexOf(this);
        if (i==-1)
            Alert.abort("unfocussable proof "+this.title);
        else
            if (i!=0) {
                focusv.remove(i);
                focusv.insertElementAt(this,0);
            }
    }

    private synchronized void removeFromfocusv() {
        int i = focusv.indexOf(this);
        if (i==-1)
            Alert.abort("unremovable proof "+this.title);
        else
            focusv.remove(i);
    }
    
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
            Alert.abort("ProofWindow.enableCopy can't find Edit: Undo/Redo");
        }

        if (japeserver.onMacOS) // put the dot in the red button
            getRootPane(). putClientProperty("windowModified",
                            (proofhistory||disproofhistory) ? Boolean.TRUE : Boolean.FALSE);
    }

    
    // the other sort of focus
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
        |                      |
        |      disproof        |
        |                      |
         ---------------------- 
        (line)
         ---------------------------
        |                           |
        |                           |
        |      proof                |
        |                           |
        |                           |
         ---------------------------
        (line)
         ---------------------
        |                     |
        |    provisos         |
        |                     |
         ---------------------
     */

    private int gap() { return 5*proofCanvas.linethickness; }

    private int separatorThickness() { return 2*proofCanvas.linethickness; }

    private void printSeparator(Graphics2D g2D, int y, int length) {
        g2D.setColor(Preferences.SeparatorColour);
        g2D.setStroke(new BasicStroke((float)separatorThickness()));
        g2D.drawLine(0, y, length, y);
    }
    
    public int print(Graphics g, PageFormat pf, int pi) throws PrinterException {
        if (pi >= 1) {
            return Printable.NO_SUCH_PAGE;
        }

        if (!(g instanceof Graphics2D)) {
            Alert.showAlert(this, Alert.Warning,
                            "Can't print: this seems to be a very old version of Java");
            return Printable.NO_SUCH_PAGE;
        }

        Graphics2D g2D = (Graphics2D) g;
        
        if (printlayout_tracing) {
            System.err.println("ProofWindow.print("+g2D+")");
            japeserver.showContainer(proofCanvas);
        }
        
        g2D.translate((int)pf.getImageableX()+1, (int)pf.getImageableY()+1);

        int printHeight=0, printWidth=0;
        Dimension disproofSize;

        // compute size of picture
        if (disproofPane!=null) {
            disproofSize = disproofPane.printSize();
            printWidth = disproofSize.width;
            printHeight = disproofSize.height+gap()+separatorThickness()+gap();
        }
        else
            disproofSize = null; // shut up compiler

        printWidth = Math.max(printWidth, proofCanvas.getWidth());
        printHeight += proofCanvas.getHeight();

        if (provisoCanvas!=null) {
            printWidth = Math.max(printWidth, provisoCanvas.getWidth());
            printHeight += gap()+separatorThickness()+gap()+provisoCanvas.getHeight();
        }

        // scale if necessary
        double scalex = (double)pf.getImageableWidth()/(double)printWidth,
               scaley = (double)pf.getImageableHeight()/(double)printHeight,
               scale = Math.min(scalex, scaley);
        AffineTransform trans = g2D.getTransform();

        if (scale<1.0) {
            System.err.println("scaling printing to "+scale);
            g2D.scale(scale, scale);
        }

        if (disproofPane!=null) {
            disproofPane.print(g);
            int liney = disproofSize.height+gap();
            printSeparator(g2D, liney, Math.max(disproofSize.width, proofCanvas.getWidth()));
            g2D.translate(0, liney+separatorThickness()+gap());
        }

        proofCanvas.paint(g);

        if (provisoCanvas!=null) {
            g2D.translate(0, proofCanvas.getHeight()+gap());
            printSeparator(g2D, 0, Math.max(proofCanvas.getWidth(), provisoCanvas.getWidth()));
            g2D.translate(0, separatorThickness()+gap());
            provisoCanvas.paint(g);
            g2D.translate(0, -(proofCanvas.getHeight()+gap()+separatorThickness()+gap()));
        }

        if (disproofPane!=null)
            g2D.translate(0, -(disproofSize.height+gap()+separatorThickness()+gap()));

        g2D.setTransform(trans);
        
        g2D.translate(-((int)pf.getImageableX()+1), -((int)pf.getImageableY()+1));
        
        return Printable.PAGE_EXISTS;
    }

    /**********************************************************************************************

        Static interface for Dispatcher

     **********************************************************************************************/

    public static ProofWindow spawn(String title, int proofnum) throws ProtocolError {
        if (findWindow(title)!=null)
            throw new ProtocolError("already a window with that title");
        else
            return new ProofWindow(title, proofnum);
    }

    private static ProofWindow findProof (int proofnum) throws ProtocolError {
        for (Enumeration e = JapeWindow.windows(); e.hasMoreElements(); ) {
            Object o = e.nextElement();
            if (o instanceof ProofWindow && ((ProofWindow)o).proofnum==proofnum)
                return (ProofWindow) o;
        }
        throw new ProtocolError("no proof numbered "+proofnum);
    }

    public static void closeproof(int proofnum) throws ProtocolError {
        ProofWindow proof = findProof(proofnum);
        proof.removeWindowListener(proof.windowListener); // Linux gives us spurious events otherwise
        proof.windowListener = null;
        proof.closeWindow();
        proof.removeFromfocusv();
        reportFocus();
        enableProofMenuItems();
    }

    private static void enableProofMenuItems() {
        if (focusv!=null && focusv.size()>0) {
            ProofWindow w = (ProofWindow)focusv.get(0);
            w.enableCopy();
            w.enableUndo();
        }
    }
    
    public static ProofWindow getFocussedProofWindow() {
        if (focusv.size()==0)
            return null;
        else
            return (ProofWindow)focusv.get(0);
    }

    public static ProofWindow mustGetFocussedProofWindow() throws ProtocolError {
        ProofWindow w = getFocussedProofWindow();
        if (w==null)
            throw new ProtocolError("no proof windows available");
        else
            return w;
    }

    private static void checkFocussedCanvas() throws ProtocolError {
        if (mustGetFocussedProofWindow().focussedCanvas==null)
            throw new ProtocolError("no focussed pane - drawInPane missing?");
    }

    private static JapeCanvas byte2JapeCanvas(byte pane, String who) throws ProtocolError {
        switch (pane) {
            case ProofPaneNum:
                return mustGetFocussedProofWindow().proofCanvas;
            case DisproofPaneNum:
                return mustGetFocussedProofWindow().ensureDisproofPane().seqCanvas;
            default:
                throw new ProtocolError(who+" pane="+pane);
        }
    }

    public static Rectangle getPaneGeometry(byte pane) throws ProtocolError {
        return byte2JapeCanvas(pane,"ProofWindow.getPaneGeometry").getViewGeometry();
    }

    public static void clearPane(byte pane) throws ProtocolError {
        // don't create a disproof pane just to clear it ...
        if (pane==DisproofPaneNum && mustGetFocussedProofWindow().disproofPane==null)
            return;
        else
            byte2JapeCanvas(pane,"ProofWindow.clearPane").removeAll();
    }

    public static void setProofParams(byte style, int linethickness) throws ProtocolError {
        mustGetFocussedProofWindow().initProofCanvas(style, linethickness);
        if (mustGetFocussedProofWindow().disproofPane!=null)
            mustGetFocussedProofWindow().disproofPane.setlinethickness(linethickness);
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
    
    public static void makeReady() {
        for (int i = 0; i<focusv.size(); i++)
            ((ProofWindow)focusv.get(i)).makeWindowReady();
    }

    public static void drawInPane(byte pane) throws ProtocolError {
        ProofWindow focussedw = mustGetFocussedProofWindow();
        switch (pane) {
            case ProofPaneNum:
                focussedw.focussedCanvas = focussedw.proofCanvas;
                break;
            case DisproofPaneNum:
                focussedw.focussedCanvas = focussedw.ensureDisproofPane().seqCanvas;
                break;
            default:
                Alert.abort("ProofWindow.drawInPane: pane="+pane);
        }
    }

    public static void drawstring(int x, int y, byte fontnum, byte kind,
                                  String annottext) throws ProtocolError {
        checkFocussedCanvas();
        JapeFont.checkInterfaceFontnum(fontnum);
        JapeCanvas canvas = mustGetFocussedProofWindow().focussedCanvas;
        switch (kind) {
            case PunctTextItem:
                canvas.add(new TextItem(canvas, x, y, fontnum, annottext));
                break;
            case HypTextItem:
                if (canvas instanceof ProofCanvas)
                    canvas.add(new HypothesisItem((ProofCanvas)canvas, x, y, fontnum, annottext));
                else
                if (canvas instanceof DisproofCanvas)
                    canvas.add(new DisproofHypItem((DisproofCanvas)canvas, x, y, fontnum, annottext));
                else
                    throw new ProtocolError("HypTextItem in "+canvas);
                break;
            case ConcTextItem:
                if (canvas instanceof ProofCanvas)
                    canvas.add(new ConclusionItem((ProofCanvas)canvas, x, y, fontnum, annottext));
                else
                if (canvas instanceof DisproofCanvas)
                    canvas.add(new DisproofConcItem((DisproofCanvas)canvas, x, y, fontnum, annottext));
                else
                    throw new ProtocolError("ConcTextItem in "+canvas);
                break;
            case AmbigTextItem:
                if (canvas instanceof ProofCanvas)
                    canvas.add(new HypConcItem((ProofCanvas)canvas, x, y, fontnum, annottext));
                else
                    throw new ProtocolError("ProofWindow.drawstring AmbigTextItem not drawing in proof pane");
                break;
            case ReasonTextItem:
                if (canvas instanceof ProofCanvas)
                    canvas.add(new ReasonItem((ProofCanvas)canvas, x, y, fontnum, annottext));
                else
                    throw new ProtocolError("ProofWindow.drawstring ReasonTextItem not drawing in proof pane");
                break;
            default:
                throw new ProtocolError("ProofWindow.drawstring kind="+kind);
        }
    }

    public static void drawRect(int x, int y, int w, int h) throws ProtocolError {
        mustGetFocussedProofWindow().proofCanvas.add(new RectItem(mustGetFocussedProofWindow().proofCanvas, x, y, w, h));
    }

    public static void drawLine(int x1, int y1, int x2, int y2) throws ProtocolError {
        mustGetFocussedProofWindow().proofCanvas.add(new LineItem(mustGetFocussedProofWindow().proofCanvas, x1, y1, x2, y2));
    }

    private static SelectableProofItem findProofSelectableXY(int x, int y) throws ProtocolError {
        SelectableProofItem si = mustGetFocussedProofWindow().proofCanvas.findSelectable(x,y);
        if (si==null)
            throw new ProtocolError("no selectable proof item at "+x+","+y);
        else
            return si;
    }

    public static void blacken(int x, int y) throws ProtocolError {
        findProofSelectableXY(x,y).blacken();
    }

    public static void greyen(int x, int y) throws ProtocolError {
        findProofSelectableXY(x,y).greyen();
    }

    public static void highlight(int x, int y, byte selclass) throws ProtocolError {
        byte selkind;
        switch (selclass) {
            case ConcTextItem  : selkind = ConcSel; break;
            case HypTextItem   : selkind = HypSel; break;
            case ReasonTextItem: selkind = ReasonSel; break;
            default            : throw new ProtocolError("ProofWindow.highlight selclass="+selclass);
        }
        findProofSelectableXY(x,y).select(selkind);
    }

    public static void unhighlight(int x, int y) throws ProtocolError {
        findProofSelectableXY(x,y).deselect();
    }

    public static void emphasise(int x, int y, boolean state) throws ProtocolError {
        EmphasisableItem ei = mustGetFocussedProofWindow().ensureDisproofPane().seqCanvas.findEmphasisable(x,y);
        if (ei==null)
            throw new ProtocolError("no emphasisable disproof item at "+x+","+y);
        else
            ei.emphasise(state);
    }
    
    public static String getSelections() throws ProtocolError {
        String s = mustGetFocussedProofWindow().proofCanvas.getSelections("\n");
        return s==null ? "" : s+"\n";
    }

    public static String getTextSelections() throws ProtocolError {
        String s = mustGetFocussedProofWindow().proofCanvas.getTextSelections("\n");
        return s==null ? "" : s+"\n";
    }

    public static String getGivenTextSelections() throws ProtocolError {
        ProofWindow w = mustGetFocussedProofWindow();
        return w.provisoPane==null ? "" : w.provisoCanvas.getTextSelections(Reply.stringSep);
    }

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

    // disproof stuff
    public static void setSequentBox(int w, int a, int d) throws ProtocolError {
        mustGetFocussedProofWindow().ensureDisproofPane().setSequentBox(w, a, d);
    }

    public static void setDisproofTiles(String[] tiles) throws ProtocolError {
        mustGetFocussedProofWindow().ensureDisproofPane().setTiles(tiles);
    }

    public static void worldsStart() throws ProtocolError {
        mustGetFocussedProofWindow().ensureDisproofPane().worldCanvas.worldsStart();
    }

    public static void addWorld(int x, int y) throws ProtocolError {
        mustGetFocussedProofWindow().ensureDisproofPane().worldCanvas.addWorld(x, y);
    }

    public static void addWorldLabel(int x, int y, String label) throws ProtocolError {
        mustGetFocussedProofWindow().ensureDisproofPane().worldCanvas.addWorldLabel(x, y, label);
    }

    public static void addChildWorld(int x, int y, int xc, int yc) throws ProtocolError {
        mustGetFocussedProofWindow().ensureDisproofPane().worldCanvas.addChildWorld(x, y, xc, yc);
    }

    public static void selectWorld(int x, int y, boolean selected) throws ProtocolError {
        mustGetFocussedProofWindow().ensureDisproofPane().worldCanvas.selectWorld(x, y, selected);
    }

    // provisos and givens

    public static void clearProvisoView() throws ProtocolError {
        // don't create a provisoPane just to clear it
        ProofWindow w = mustGetFocussedProofWindow();
        if (w.provisoCanvas!=null) {
            w.provisoCanvas.clear();
        }
    }

    public static void showProvisoLine(String annottext) throws ProtocolError {
        ProofWindow w = mustGetFocussedProofWindow();
        w.ensureProvisoPane();
        w.provisoCanvas.addProvisoLine(annottext);
    }

    public static void setGivens(MiscellaneousConstants.IntString[] gs) throws ProtocolError {
        // don't create a provisoPane just to say there aren't any givens
        ProofWindow w = mustGetFocussedProofWindow();
        if (w.provisoPane!=null || (gs!=null && gs.length!=0)) {
            w.ensureProvisoPane();
            w.provisoCanvas.setGivens(gs);
        }
    }

    /* because the engine thinks there is only one menu, we have to do clever things
        with undo and redo
        */

    private static boolean proofhistory, prooffuture, disproofhistory, disprooffuture;

    public static void setHistoryVar(boolean isUndo, boolean isProof, boolean enable) {
        if (isUndo) {
            if (isProof) proofhistory = enable;
            else         disproofhistory = enable;
        }
        else {
            if (isProof) prooffuture = enable;
            else         disprooffuture = enable;
        }

        enableProofMenuItems();
    }
                                                       }
