//
//  ProofWindow.java
//  japeserver
//
//  Created by Richard Bornat on Sat Aug 10 2002.
//  Copyleft 2002 Richard Bornat & Bernard Sufrin. Proper GPL text to be inserted
//

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import javax.swing.JFrame;
import javax.swing.JMenuBar;

public class ProofWindow extends JapeWindow {
    static final String message = "Hello World!";
    private Font font = new Font("serif", Font.ITALIC+Font.BOLD, 36);

    public ProofWindow() {
        super("japeserver");
        this.getContentPane().setLayout(null);
        setJMenuBar(new JMenuBar()); // by experiment, seems to be necessary before setVisible
        setVisible(true);
    }

    public void paint(Graphics g) {
        super.paint(g);
        g.setColor(Color.blue);
        g.setFont (font);
        g.drawString(message, 40, 80);
    }


}
