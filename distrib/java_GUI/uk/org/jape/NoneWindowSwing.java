/*
 * Adapted from the Sun java tutorial example.
 *
 * A brand new attempt, derived from a bit of (belated) thought and a subsequent sleepless night.
 *
 * For the time being, no attempt to scroll the panel.
 */

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

public class NoneWindowSwing extends JFrame implements ActionListener {
    boolean inAnApplet = true;
    private JButton bN, bE, bS, bW;

    /*
     * The layered mechanism of Java: a JScrollPane containing a JViewport containing a JPanel
     * (or some JComponent) containing ... - makes it difficult to produce nice UI effects.
     *
     * The default behaviour of a JScrollPane+JViewport is to show as much of the viewed component
     * as possible, and to put its top left corner top left of the viewport and (of course) not
     * to let you scroll off the top of what you are looking at.  Bad for me when I want to
     * display a proof which consists of a couple of lines, and anchor its bottom left corner
     * to the bottom left of the viewport.
     *
     * This can only be fixed by setting the size of the viewed component so as to force it to
     * appear where I want it to.  That means that the contained component has to know the
     * size of the viewport, and know when it is resized.  I'm still unsure about the best
     * way to do that ...
     */

    private ContainerWithOrigin panel;
    private AnchoredScrollPane scrollPane;
    
    public NoneWindowSwing() {
        String yes = "Yes", no = "No";
        String[] buttons = { yes, no };

        panel = new ContainerWithOrigin();
        
        bN = new JButton("North"); bN.setBounds(25, 5, 75, 20); bN.addActionListener(this); panel.add(bN);
        bE = new JButton("East"); bE.setBounds(55, 35, 75, 20); bE.addActionListener(this); panel.add(bE);
        bS = new JButton("South"); bS.setBounds(150, 15, 75, 30); bS.addActionListener(this); panel.add(bS);
        bW = new JButton("West"); bW.setBounds(150, -5, 75, 30); bW.addActionListener(this); panel.add(bW);

        // panel.setBorder(new SoftBevelBorder(SoftBevelBorder.LOWERED, Color.red, Color.blue));
        panel.setBackground(Color.yellow);
        panel.setLocation(100,100);

        panel.computeBounds();

        Container contentPane = getContentPane();
        Rectangle panelBounds = panel.getBounds();

        scrollPane = new AnchoredScrollPane();
        scrollPane.add(panel, AnchoredScrollPane.ANCHOR_SOUTH);
        
        contentPane.add(scrollPane);
        contentPane.doLayout();

        // nothing seems to give us a properly-sized window ...
        //setSize(Math.min(500, Math.max(200,panelBounds.x+panelBounds.width+50)),
        //        Math.min(400, Math.max(200, panelBounds.y+panelBounds.height+50)));
        
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                if (inAnApplet) {
                    dispose();
                } else {
                    System.exit(0);
                }
            }
        });
    }
    
    public void actionPerformed(ActionEvent e) {
    	String s = e.getActionCommand();
        if (s.equals("North")) {
            Point p = bN.getLocation();
            p.y -= 50; bN.setLocation(p);
        }
        else
        if (s.equals("East")) {
            Point p = bE.getLocation();
            p.x += 50; bE.setLocation(p);
        }
        else
        if (s.equals("South")) {
            Point p = bS.getLocation();
            p.y += 50; bS.setLocation(p);
        }
        else
        if (s.equals("West")) {
            Point p = bW.getLocation();
            p.x -= 50; bW.setLocation(p);
        }

        panel.computeBounds();
    }

    public static void main(String args[]) {
        String lNf = "javax.swing.plaf.metal.MetalLookAndFeel";
        try {
            // UIManager.setLookAndFeel(lNf);
        } catch (Exception e) {
            System.err.println("couldn't set L&F "+lNf);
        }
        NoneWindowSwing window = new NoneWindowSwing();
        window.inAnApplet = false; /* really? */

        window.setTitle("Absolute Positioning with negative coordinates in a panel");
        // window.pack(); // gives a zero-dimensioned pane, I kid you not.
        window.setVisible(true);
    }
}
