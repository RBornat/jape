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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import javax.swing.JFrame;
import javax.swing.JMenuBar;

public class ProofWindow extends JapeWindow {
    int proofnum;
    protected static ProofWindow focussedproof = null;
    
    public ProofWindow(String title, int proofnum) {
        super(title);
        this.proofnum = proofnum;
        this.getContentPane().setLayout(null);
        setJMenuBar(new JMenuBar()); // by experiment, seems to be necessary before setVisible
        setVisible(true);
        focussedproof = this;
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
