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
import java.awt.Font;
import java.awt.Graphics;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuBar;
import javax.swing.JScrollPane;
import java.awt.Point;
import java.awt.Rectangle;

public class ProofWindow extends JapeWindow implements ProtocolConstants {
    int proofnum;
    protected static ProofWindow focussedProofWindow = null;

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
        setBar(); 
        pack();
        setVisible(true);
        focussedProofWindow = this;
    }
    
    public static ProofWindow spawn(String title, int proofnum) throws ProtocolError {
        if (findWindow(title)!=null)
            throw new ProtocolError("already a window with that title");
        else
            return new ProofWindow(title, proofnum);
    }
    
    public static ProofWindow focussedProofWindow() {
        // for now, I only deal with one proof window
        return focussedProofWindow;
    }

    private static void checkFocussedProofWindow() throws ProtocolError {
        if (focussedProofWindow==null)
            throw new ProtocolError("no focussed proof");
    }

    private static void checkFocussedCanvas() throws ProtocolError {
        checkFocussedProofWindow();
        if (focussedProofWindow.focussedCanvas==null)
            throw new ProtocolError("no focussed pane - drawInPane missing?");
    }

    /* from displayfont.mli:
       (* Useful translation for Japeserver marshalling.
        *
        *  ProofPane = 0
        *  DisproofPane = 1
        *
        *)
     */

    public static final byte ProofPaneNum = 0, DisproofPaneNum = 1;

    private static JapeCanvas byte2JapeCanvas(byte pane, String who) throws ProtocolError {
        checkFocussedProofWindow();
        switch (pane) {
            case ProofPaneNum:
                return focussedProofWindow.proofCanvas; 
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
        checkFocussedProofWindow();
        focussedProofWindow.initProofCanvas(style, linethickness);
        if (focussedProofWindow.disproofCanvas!=null)
            focussedProofWindow.disproofCanvas.linethickness = linethickness;
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
        checkFocussedProofWindow();
        switch (pane) {
            case ProofPaneNum:
                focussedProofWindow.focussedCanvas = focussedProofWindow.proofCanvas;
                break;
            case DisproofPaneNum:
                throw new ProtocolError("ProofWindow.drawInPane: no disproofCanvas support yet");
            default:
                Alert.abort("ProofWindow.drawInPane: pane="+pane);
        }
    }

    public static void setGivens(String[] gs) throws ProtocolError {
        checkFocussedProofWindow();
        focussedProofWindow.newGivens(gs);
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
        JapeCanvas canvas = focussedProofWindow.focussedCanvas;
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

    public static String getSelections() throws ProtocolError {
        checkFocussedProofWindow();
        return focussedProofWindow.proofCanvas.getSelections();
    }

    public static String getTextSelections() throws ProtocolError {
        checkFocussedProofWindow();
        return focussedProofWindow.proofCanvas.getTextSelections();
    }

    public static String getGivenTextSelections() throws ProtocolError {
        checkFocussedProofWindow();
        if (focussedProofWindow.provisoPane!=null)
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
