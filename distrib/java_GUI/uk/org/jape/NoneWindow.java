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

    private class KViewport extends JViewport {
        KViewport() {
            super();
        }
        private void paintView(Graphics g) {
            System.err.println("in paintView: position="+getViewPosition()+"; g="+g);
            Rectangle r = g.getClipBounds();
            g.setClip(r.x, r.y, r.width, r.height);
            JComponent view = (JComponent)getView();
            int x = view.getX();
            int y = view.getY();
            System.err.println("view is "+view+"; panel is "+panel);
            System.err.println("view has bounds "+view.getBounds());
            g.translate(x, y);
            if (view.getWidth() < r.width)  {
                System.err.println("filling in background 0,0,"+r.width+","+r.height);
                g.setColor(getBackground());
                g.fillRect(0, 0, r.width, r.height);
            }
            view.paint(g);
            g.translate(-x, -y); // needed !!
        /*
            Rectangle r = g.getClipBounds();
            RepaintManager rm = RepaintManager.currentManager(this);
            boolean dblbEnable = false;//rm.isDoubleBufferingEnabled();BURKEY JCK COMPLIANCE BUT IGNORE isDoubleBufferingEnabled
            JComponent view = (JComponent) getView();
            r.x -= view.getX();
            r.y -= view.getY();
            Image off = rm.getOffscreenBuffer(this,r.width,r.height);
            Graphics og = off.getGraphics();
            if (view.getWidth() < r.width)
            {
                og.setColor(getBackground());
                og.fillRect(0, 0, r.width, r.height);
            }
            og.translate(-r.x,-r.y);
            og.setClip(r.x,r.y,r.width,r.height);
            rm.setDoubleBufferingEnabled(false);
            view.paint(og);
            if (dblbEnable)
            {
                rm.setDoubleBufferingEnabled(true);
            }
            g.drawImage(off,r.x + view.getX(),r.y + view.getY(),null);
            og.dispose();
            */
        }

        public void paint(Graphics g) {
            paintView(g);
            // super.paint(g);
        }
    }

    private JScrollablePanel panel;
    private JScrollPane scrollPane;

    public NoneWindow() {
        panel = new JScrollablePanel();
        panel.setLayout(null);
        panel.addComponentListener(new ComponentAdapter() {
            public void componentMoved(ComponentEvent e) {
                System.err.println("componentMoved to "+panel.getLocation());
            }
            public void componentResized(ComponentEvent e) {
                System.err.println("componentResized to "+panel.getSize());
            }
        });
        
        b1 = new JButton("one"); b1.addActionListener(this); panel.add(b1);
        b2 = new JButton("two"); b2.addActionListener(this); panel.add(b2);
        b3 = new JButton("three"); b3.addActionListener(this); panel.add(b3);

        KViewport kv = new KViewport();
        kv.setView(panel);
        
        scrollPane = new JScrollPane();
        scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        scrollPane.setViewport(kv);
        
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
        r = b1.getBounds();
        r.add(b2.getBounds()); r.add(b3.getBounds());
        System.err.println("setting bounds "+r);
        panel.setSize(r.width,r.height);
        panel.setLocation(r.x,r.y);
        System.err.println("before revalidate bounds are "+panel.getBounds());
        panel.revalidate();
    	// this seems to be the only way to tell a scroll pane what to put in its bars 
    	// (http://java.sun.com/docs/books/tutorial/uiswing/components/problems.html and elsewhere)
    	panel.setPreferredSize(panel.getSize());
    	panel.revalidate();
        //scrollPane.getViewport().revalidate();
        System.err.println("bounds are now "+panel.getBounds());
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
