/*
 * Swing version.
 */

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class NoneWindow extends JFrame implements ActionListener {
    boolean inAnApplet = true;
    private boolean laidOut = false;
    private JButton b1, b2, b3;
    
    private class JScrollablePanel extends JPanel implements Scrollable {
		public Dimension getPreferredScrollableViewportSize() {
			System.err.println("they asked the panel for its preferred size");
			return getPreferredSize();
		}
	
		public int getScrollableUnitIncrement(Rectangle visibleRect,
											  int orientation,
											  int direction) {
			return 10; // for now
		}
	
		public int getScrollableBlockIncrement(Rectangle visibleRect,
											   int orientation,
											   int direction) {
			return 100; // for now
		}
	
		public boolean getScrollableTracksViewportWidth() {
			return false;
		}
	
		public boolean getScrollableTracksViewportHeight() {
			return false;
		}
    }
    
    private JScrollablePanel panel;
	private JScrollPane scrollPane;
	
    public NoneWindow() {
    	panel = new JScrollablePanel();
        panel.setLayout(null);

        b1 = new JButton("one");
        b1.addActionListener(this);
        panel.add(b1);
        b2 = new JButton("two");
        b2.addActionListener(this);
        panel.add(b2);
        b3 = new JButton("three");
        b3.addActionListener(this);
        panel.add(b3);
        
        scrollPane = new JScrollPane(panel, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                                            JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        getContentPane().add(scrollPane);

        Insets insets = getContentPane().getInsets();
        b1.setBounds(25 + insets.left, 5 + insets.top, 75, 20);
        b2.setBounds(55 + insets.left, 35 + insets.top, 75, 20);
        b3.setBounds(150 + insets.left, 15 + insets.top, 75, 30);

		computeBounds();
		panel.revalidate();
		
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
    	r.add(b2.getBounds());
    	r.add(b3.getBounds());
    	System.err.print("computeBounds "+r+"+"+panel.getBounds());
    	panel.setPreferredSize(r.getSize());
    	panel.setBounds(r.union(panel.getBounds()));
    	System.err.println(" "+r+" "+scrollPane.getViewport().getViewPosition());
    	scrollPane.getViewport().setViewPosition(new Point(0,0));
    	System.err.println(scrollPane.getViewport().getViewPosition());
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
		else
			System.err.println("action command "+s);
		computeBounds();
    	panel.revalidate();
    	panel.repaint();
    }

    public static void main(String args[]) {
        NoneWindow window = new NoneWindow();
        Insets insets = window.getInsets();
        window.inAnApplet = false;

        window.setTitle("Absolute Positioning");
        window.setSize(250 + insets.left + insets.right,
                       90 + insets.top + insets.bottom);
        window.setVisible(true);
    }
}
