//
//  $Id$
//
//  Copyleft 2002 Richard Bornat & Bernard Sufrin. Proper GPL text to be inserted
//

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
