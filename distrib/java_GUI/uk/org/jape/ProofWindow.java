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
import javax.swing.JMenuBar;
import javax.swing.JScrollPane;
import java.awt.Point;

public class ProofWindow extends JapeWindow {
    int proofnum;
    protected static ProofWindow focussedproof = null;

    protected ProofCanvas canvas; // Sun say use JPanel, but for now ...
    protected JScrollPane proofpane;
    
    public ProofWindow(String title, int proofnum) {
        super(title);
        this.proofnum = proofnum;

        getContentPane().setLayout(new BorderLayout()); 
        canvas = new ProofCanvas();
        canvas.setSize(LocalSettings.proofPanelDefaultSize);
        proofpane = new JScrollPane(canvas, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                                            JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        getContentPane().add(proofpane, BorderLayout.CENTER);
        setBar(); 
        pack();
        setVisible(true);
        focussedproof = this;

        TextItem t = new TextItem(canvas, new Point(50, 50), "foobaz", 2);
        t.selected=true;
        canvas.registerItem(t);
        t = new TextItem(canvas, new Point(50, 100), "is best for you", 2);
        t.selected=true;
        t.greyed=true;
        canvas.registerItem(t);
        t = new TextItem(canvas, new Point(50, 150), "on \u22d6 April \u22d7 Thursdays", 2);
        canvas.registerItem(t);
        canvas.repaint();
    }
    
    public static ProofWindow spawn(String title, int proofnum) throws ProtocolError {
        if (findWindow(title)!=null)
            throw new ProtocolError("already a window with that title");
        else
            return new ProofWindow(title, proofnum);
    }
    
    public static ProofWindow focussedProofWindow() {
        // for now, I only deal with one proof window
        return focussedproof;
    }
    
    public static void tellDimension() throws ProtocolError {
        if (focussedproof==null) 
            throw new ProtocolError("no focussed proof");
        else {
            Dimension d = focussedproof.getSize();
            Reply.reply("0 0 "+d.width+" "+d.height);
        }
    }
}
