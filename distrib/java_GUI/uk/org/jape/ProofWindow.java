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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.util.Enumeration;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.Font;
import java.awt.Graphics;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuBar;
import javax.swing.JScrollPane;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Vector;

public class ProofWindow extends JapeWindow implements SelectionConstants, ProtocolConstants {
    public final int proofnum;

    protected AnchoredScrollPane proofPane;
    protected ProofCanvas proofCanvas;
    protected AnchoredScrollPane disproofPane;
    protected JapeCanvas disproofCanvas;
    protected AnchoredScrollPane provisoPane;
    protected JapeCanvas provisoCanvas;

    protected JapeCanvas focussedCanvas;
    
    public ProofWindow(String title, int proofnum) {
        super(title);
        this.proofnum = proofnum;

        getContentPane().setLayout(new BorderLayout()); 
        proofPane = new AnchoredScrollPane();
        proofCanvas = new ProofCanvas();
        proofPane.add(proofCanvas);
        
        getContentPane().add(proofPane, BorderLayout.CENTER);

        setSize(LocalSettings.DefaultProofWindowSize);
        setLocation(nextPos());
        
        focusv.insertElementAt(this, 0);
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                Reply.sendCOMMAND("closeproof "+ProofWindow.this.proofnum);
            }
            public void windowActivated(WindowEvent e) {
                int i = focusv.indexOf(ProofWindow.this);
                if (i==-1)
                    Alert.abort("unfocussable window "+ProofWindow.this.title);
                else
                    if (i!=0) {
                        focusv.remove(i);
                        focusv.insertElementAt(ProofWindow.this,0);
                        reportFocus();
                    }
            }
        });


        setBar();
        pack();
        setVisible(true);
    }

    public boolean equals(Object o) {
        return o instanceof ProofWindow ? ((ProofWindow)o).title.equals(title) &&
                                               ((ProofWindow)o).proofnum==proofnum :
                                          super.equals(o);
    }

    private static Vector focusv = new Vector();

    private static void reportFocus() {
        if (focusv.size()!=0)
            Reply.sendCOMMAND("setfocus "+((ProofWindow)focusv.get(0)).proofnum);
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
        focusv.remove(focusv.indexOf(proof));
        proof.dispose();
        reportFocus();
    }

    public static ProofWindow focussedProofWindow(boolean musthave) throws ProtocolError {
        if (focusv.size()==0) {
            if (musthave)
                throw new ProtocolError("no proof windows available");
            else
                return null;
        }
        else
            return (ProofWindow)focusv.get(0);
    }

    private static void checkFocussedCanvas() throws ProtocolError {
        if (focussedProofWindow(true).focussedCanvas==null)
            throw new ProtocolError("no focussed pane - drawInPane missing?");
    }

    private static JapeCanvas byte2JapeCanvas(byte pane, String who) throws ProtocolError {
        switch (pane) {
            case ProofPaneNum:
                return focussedProofWindow(true).proofCanvas; 
            case DisproofPaneNum:
                throw new ProtocolError(who+": no disproofCanvas support yet"); 
            default:
                throw new ProtocolError(who+" pane="+pane);
        }
    }

    public static Rectangle getPaneGeometry(byte pane) throws ProtocolError {
        return byte2JapeCanvas(pane,"ProofWindow.getPaneGeometry").viewGeometry();
    }

    public static void clearPane(byte pane) throws ProtocolError {
        byte2JapeCanvas(pane,"ProofWindow.clearPane").clearPane();
    }

    public static void setProofParams(byte style, int linethickness) throws ProtocolError {
        focussedProofWindow(true).initProofCanvas(style, linethickness);
        if (focussedProofWindow(true).disproofCanvas!=null)
            focussedProofWindow(true).disproofCanvas.linethickness = linethickness;
    }

    private void initProofCanvas(byte style, int linethickness) {
        switch(style) {
            case BoxStyle:
                proofPane.setanchor(proofPane.ANCHOR_SOUTHWEST); break;
            case TreeStyle:
                proofPane.setanchor(proofPane.ANCHOR_SOUTH); break;
            default:
                Alert.abort("ProofWindow.initProofCanvas style="+style);
        }
        proofPane.validate(); proofPane.repaint();
        focussedCanvas = proofCanvas; // really?
        proofCanvas.proofStyle = style;
        proofCanvas.linethickness = linethickness;
    }

    private void newDisproofCanvas() throws ProtocolError {
        throw new ProtocolError("ProofWindow.newDisproofCanvas: not yet");
    }

    public static void drawInPane(byte pane) throws ProtocolError {
        switch (pane) {
            case ProofPaneNum:
                focussedProofWindow(true).focussedCanvas = focussedProofWindow(true).proofCanvas;
                break;
            case DisproofPaneNum:
                throw new ProtocolError("ProofWindow.drawInPane: no disproofCanvas support yet");
            default:
                Alert.abort("ProofWindow.drawInPane: pane="+pane);
        }
    }

    public static void setGivens(String[] gs) throws ProtocolError {
        focussedProofWindow(true).newGivens(gs);
    }

    private void newGivens(String[] gs) throws ProtocolError {
        if (gs.length!=0)
            throw new ProtocolError("can't (yet) set givens "+gs);
    }

    public static void clearProvisoView() throws ProtocolError {
        // do nothing for the time being
    }

    public static void drawstring(int x, int y, byte fontnum, byte kind,
                                  String annottext, String printtext) throws ProtocolError {
        checkFocussedCanvas();
        JapeFont.checkInterfaceFontnum(fontnum);
        JapeCanvas canvas = focussedProofWindow(true).focussedCanvas;
        switch (kind) {
            case PunctTextItem:
                canvas.add(new TextItem(canvas, x, y, fontnum, annottext, printtext)); break;
            case HypTextItem:
                if (canvas instanceof ProofCanvas) {
                    canvas.add(new HypothesisItem((ProofCanvas)canvas, x, y, fontnum, annottext, printtext)); break;
                }
                else
                    throw new ProtocolError("ProofWindow.drawstring HypTextItem not drawing in proof pane");
            case ConcTextItem:
                if (canvas instanceof ProofCanvas) {
                    canvas.add(new ConclusionItem((ProofCanvas)canvas, x, y, fontnum, annottext, printtext)); break;
                }
                else
                    throw new ProtocolError("ProofWindow.drawstring ConcTextItem not drawing in proof pane");
            case AmbigTextItem:
                if (canvas instanceof ProofCanvas) {
                    canvas.add(new HypConcItem((ProofCanvas)canvas, x, y, fontnum, annottext, printtext)); break;
                }
                else
                    throw new ProtocolError("ProofWindow.drawstring AmbigTextItem not drawing in proof pane");
            case ReasonTextItem:
                if (canvas instanceof ProofCanvas) {
                    canvas.add(new ReasonItem((ProofCanvas)canvas, x, y, fontnum, annottext, printtext)); break;
                }
                else
                    throw new ProtocolError("ProofWindow.drawstring ReasonTextItem not drawing in proof pane");
            default:
                throw new ProtocolError("ProofWindow.drawstring kind="+kind);
        }
    }

    public static void drawRect(int x, int y, int w, int h) throws ProtocolError {
        focussedProofWindow(true).proofCanvas.add(new RectItem(focussedProofWindow(true).proofCanvas, x, y, w, h));
    }

    public static void drawLine(int x1, int y1, int x2, int y2) throws ProtocolError {
        focussedProofWindow(true).proofCanvas.add(new LineItem(focussedProofWindow(true).proofCanvas, x1, y1, x2, y2));
    }

    private static SelectableProofItem findSelectableXY(int x, int y) throws ProtocolError {
        SelectableProofItem si = focussedProofWindow(true).proofCanvas.findSelectableXY(x,y);
        if (si==null)
            throw new ProtocolError("no blackenable item at "+x+","+y);
        else
            return si;
    }

    public static void blacken(int x, int y) throws ProtocolError {
        findSelectableXY(x,y).blacken();
    }

    public static void greyen(int x, int y) throws ProtocolError {
        findSelectableXY(x,y).greyen();
    }

    public static void highlight(int x, int y, byte selclass) throws ProtocolError {
        byte selkind;
        switch (selclass) {
            case ConcTextItem  : selkind = ConcSel; break;
            case HypTextItem   : selkind = HypSel; break;
            case ReasonTextItem: selkind = ReasonSel; break;
            default            : throw new ProtocolError("ProofWindow.highlight selclass="+selclass);
        }
        findSelectableXY(x,y).select(selkind);
    }

    public static void unhighlight(int x, int y) throws ProtocolError {
        findSelectableXY(x,y).deselect();
    }

    public static String getSelections() throws ProtocolError {
        String s = focussedProofWindow(true).proofCanvas.getSelections("\n");
        return s==null ? "" : s+"\n";
    }

    public static String getTextSelections() throws ProtocolError {
        String s = focussedProofWindow(true).proofCanvas.getTextSelections("\n");
        return s==null ? "" : s+"\n";
    }

    public static String getGivenTextSelections() throws ProtocolError {
        if (focussedProofWindow(true).provisoPane!=null)
            Alert.abort("no support for Given Text Selections");
        return "";
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
}
