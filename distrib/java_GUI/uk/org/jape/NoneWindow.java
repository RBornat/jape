/*
 * Adapted from the Sun java tutorial example.
 *
 * The 'three' button moves SouthEast when clicked; the scroll pane copes fine.
 * 
 * The 'two' button moves West; it's drawn in the right position but when it 
 * crosses the western boundary (the y axis) the scroll pane doesn't indicate
 * properly.  Instead the _eastern_ extent shown as panel (viewport?) background
 * is increased (i.e the scroll pane is responding to getPreferredSize but not 
 * to getBounds).
 *
 * The 'one' button moves North, and shows similar results.
 *
 * Debug printing in computeBounds shows that the viewport knows it isn't starting
 * from 0,0.  Is this a coordinate transformation problem?
 *
 */

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class NoneWindow extends JFrame implements ActionListener {
    boolean inAnApplet = true;
    private JButton b1, b2, b3;
    
    private class JScrollablePanel extends JPanel implements Scrollable {
		public Dimension getPreferredScrollableViewportSize() {
			return getPreferredSize();
		}
	
		public int getScrollableUnitIncrement(Rectangle v, int o, int d) {
			return 10; // for now
		}
	
		public int getScrollableBlockIncrement(Rectangle v, int o, int d) {
			return 100; // for now
		}
	
		public boolean getScrollableTracksViewportWidth() {
			return false; // so we can see the extent of the panel
		}
	
		public boolean getScrollableTracksViewportHeight() {
			return false; // ditto
		}
    }
    
    private JScrollablePanel panel;
	private JScrollPane scrollPane;
	
    public NoneWindow() {
    	panel = new JScrollablePanel();
        panel.setLayout(null);

        b1 = new JButton("one"); b1.addActionListener(this); panel.add(b1);
        b2 = new JButton("two"); b2.addActionListener(this); panel.add(b2);
        b3 = new JButton("three"); b3.addActionListener(this); panel.add(b3);
        
        scrollPane = new JScrollPane(panel, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                                            JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        getContentPane().add(scrollPane);

        Insets insets = getContentPane().getInsets();
        b1.setBounds(25 + insets.left, 5 + insets.top, 75, 20);
        b2.setBounds(55 + insets.left, 35 + insets.top, 75, 20);
        b3.setBounds(150 + insets.left, 15 + insets.top, 75, 30);

        computeBounds();
		
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
    
    protected void computeBounds() {
    	Rectangle r = new Rectangle(0,0,0,0); 
        r.add(b1.getBounds());
        // r = b1.getBounds();
        r.add(b2.getBounds()); r.add(b3.getBounds());
        panel.setBounds(r);
        panel.revalidate();
    	// this seems to be the only way to tell a scroll pane what to put in its bars 
    	// (http://java.sun.com/docs/books/tutorial/uiswing/components/problems.html and elsewhere)
    	panel.setPreferredSize(panel.getSize());
    	panel.revalidate();
        scrollPane.getViewport().revalidate();
    }
    
    public void actionPerformed(ActionEvent e) {
    	String s = e.getActionCommand();
        if (s.equals("three")) {
                Rectangle r = b3.getBounds();
                b3.setBounds(r.x+50, r.y+50, r.width, r.height);
        }
        else
        if (s.equals("two")) {
                Rectangle r = b2.getBounds();
                b2.setBounds(r.x-50, r.y, r.width, r.height);
        }
        else
        if (s.equals("one")) {
                Rectangle r = b1.getBounds();
                b1.setBounds(r.x, r.y-50, r.width, r.height);
        }

        computeBounds();
    	panel.repaint();
    }

    public static void main(String args[]) {
        try {
            // UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
        } catch (Exception e) {
            System.err.println("couldn't set L&F javax.swing.plaf.metal.MetalLookAndFeel");
        }
        NoneWindow window = new NoneWindow();
        Insets insets = window.getInsets();
        window.inAnApplet = false;

        window.setTitle("Absolute Positioning");
        window.setVisible(true);
    }
}
