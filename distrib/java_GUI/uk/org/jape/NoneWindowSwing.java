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
import javax.swing.border.*;

public class NoneWindowSwing extends JFrame implements ActionListener {
    boolean inAnApplet = true;
    private JButton b1, b2, b3;

    /*
     * The problem is that JViewport (and, no doubt, all its little UI helpers) assumes that the
     * origin of any JComponent it contains is 0,0.  To compound the error, it alters the location
     * of the client to record the scrolling position (oh dear!).  So I maintain a separate
     * origin, which begins to solve some of the problems.
     */

    /*
     * I thought I was getting somewhere, but looking at the blithering nonsense it tells me about
     * the local->screen coordinate conversion, I can no longer believe it can be made to work.
     * Certainly I'll never get hit detection to work.
     */ 
    
    private class KPanel extends JPanel implements Scrollable {
        public Dimension getPreferredScrollableViewportSize() { return getPreferredSize(); }

        public int getScrollableUnitIncrement(Rectangle v, int o, int d) { return 10; } // for now 

        public int getScrollableBlockIncrement(Rectangle v, int o, int d) { return 100; } // for now

        public boolean getScrollableTracksViewportWidth() { return false; } // so we can see the extent of the panel

        public boolean getScrollableTracksViewportHeight() { return false; } // ditto

        public void setLocation(int x, int y) {
            if (placeBackground || placeButtons)
                System.err.println("panel setLocation "+x+","+y);
            super.setLocation(x,y);
        }

        public void setLocation(Point p) {
            if (placeBackground || placeButtons)
                System.err.println("panel setLocation "+p);
            super.setLocation(p);
        }

        public void setBounds(Rectangle r) {
            if (placeBackground || placeButtons)
                System.err.println("panel setBounds "+r);
            super.setBounds(r);
        }

        public void setBounds(int x, int y, int w, int h) {
            if (placeBackground || placeButtons)
                System.err.println("panel setBounds "+x+","+y+","+w+","+h);
            super.setBounds(x,y,w,h);
        }

        public void setSize(int width, int height) {
            if (placeBackground || placeButtons)
                System.err.println("setting size to "+width+","+height);
            super.setSize(width,height);
        }

        public void setSize(Dimension d) {
            if (placeBackground || placeButtons)
                System.err.println("setting size to "+d);
            super.setSize(d);
        }

        Point origin = new Point(0,0);
        
        public void setOrigin(int x, int y) {
            if (placeBackground || placeButtons)
                System.err.println("setting origin to "+x+","+y);
            origin.x=x; origin.y=y;
            super.setLocation(x,y); // needed -- without it you don't get proper performance in the SE sector
        }

        public void setTrueBounds(Rectangle r) {
            setOrigin(r.x,r.y);
            setSize(r.width, r.height);
        }

        Point lastPaintedOrigin = null;
        Point lastPaintedViewPosition = null;
        Dimension lastPaintedSize = null;
        Shape lastPaintedClip = null;
        
        private boolean mustshow(Graphics g) {
            Point viewPosition = scrollPane.getViewport().getViewPosition();
            Dimension size = getSize();
            Shape clip = g.getClip();
            boolean show = placeBackground || placeButtons && (lastPaintedOrigin==null ||
                                         !lastPaintedOrigin.equals(origin) ||
                                         !lastPaintedViewPosition.equals(viewPosition) ||
                                         !lastPaintedSize.equals(size) ||
                                         !lastPaintedClip.equals(clip));
            if (show) {
                if (lastPaintedOrigin==null) {
                    lastPaintedOrigin = new Point(origin.x, origin.y);
                    lastPaintedViewPosition=new Point(viewPosition.x,viewPosition.y);
                    lastPaintedSize = new Dimension(size.width, size.height);
                    lastPaintedClip = clip;
                }
                else {
                    lastPaintedOrigin.x=origin.x; lastPaintedOrigin.y=origin.y;
                    lastPaintedViewPosition.x=viewPosition.x; lastPaintedViewPosition.y=viewPosition.y;
                    lastPaintedSize.width = size.width; lastPaintedSize.height = size.height;
                    lastPaintedClip = clip;
                }
            }
            return show;
        }

        public void paint(Graphics g) {
            if (placeBackground) {
                boolean show = mustshow(g);
                if (show) {
                    Point viewPosition = scrollPane.getViewport().getViewPosition();
                    Point p = new Point(0,0);
                    SwingUtilities.convertPointToScreen(p, this);
                    System.err.print("paint("+g+") at "+viewPosition.x+","+viewPosition.y+", clip="+g.getClip()+
                                     ", bounds="+getBounds()+", origin at "+p);
                }
                Graphics g1 = g.create(origin.x, origin.y, getWidth(), getHeight());
                if (show) {
                    System.err.println(", translated to "+g1+", clip="+g1.getClip());
                }
                super.paint(g1);
                g1.dispose();
            }
            else
                super.paint(g);
        }

        protected void paintChildren(Graphics g) {
            if (placeButtons) {
                g.translate(-origin.x, -origin.y);
                super.paintChildren(g);
                g.translate(origin.x,origin.y);
            }
            else
                super.paintChildren(g);
        }
    }

    private class KViewport extends JViewport {

        KViewport() {
            super();
        }

        // this code copied from JViewport, here to override private restriction
        private void paintView(Graphics g)
        {
            Rectangle r = g.getClipBounds();
            g.setClip(r.x, r.y, r.width, r.height);

            JComponent view = (JComponent)getView();
            int x = view.getX();
            int y = view.getY();
            g.translate(x, y);
            if (paintViewBackground || view.getWidth() < r.width) {
                g.setColor(getBackground());
                g.fillRect(0, 0, r.width, r.height);
            }
            view.paint(g);
            g.translate(-x, -y); // needed !!
        }
        
        public void paint(Graphics g) {
            if (useBlit)
                super.paint(g);
            else
                paintView(g);
        }
    }

    private class KScrollPane extends JScrollPane {
        KScrollPane() {
            super();
        }
        // experiments in here so far unsuccessful.
    }
    
    private KPanel panel;
    private KScrollPane scrollPane;

    private final boolean placeBackground, placeButtons, useBlit, paintViewBackground;

    public NoneWindowSwing() {
        String yes = "Yes", no = "No";
        String[] buttons = { yes, no };
        placeBackground = JOptionPane.showOptionDialog(null, "attempt to place panel background correctly?", null, 0,
                                     JOptionPane.QUESTION_MESSAGE,null,buttons, no)==0;
        placeButtons = JOptionPane.showOptionDialog(null, "attempt to place buttons relative to background?", null, 0,
                                                    JOptionPane.QUESTION_MESSAGE,null,buttons, no)==0;
        useBlit = JOptionPane.showOptionDialog(null, "use background painting store?", null, 0,
                                               JOptionPane.QUESTION_MESSAGE,null,buttons, yes)==0;
        paintViewBackground = !useBlit && JOptionPane.showOptionDialog(null, "always colour viewport?", null, 0,
                                                           JOptionPane.QUESTION_MESSAGE,null,buttons, no)==0;
        panel = new KPanel();
        panel.setLayout(null);
        
        b1 = new JButton("one"); b1.addActionListener(this); panel.add(b1);
        b2 = new JButton("two"); b2.addActionListener(this); panel.add(b2);
        b3 = new JButton("three"); b3.addActionListener(this); panel.add(b3);

        b1.setBounds(25, 5, 75, 20);
        b2.setBounds(55, 35, 75, 20);
        b3.setBounds(150, 15, 75, 30);

        panel.setBorder(new SoftBevelBorder(SoftBevelBorder.LOWERED, Color.red, Color.blue));
        panel.setBackground(Color.yellow);
        computeBounds();
        
        KViewport kv = new KViewport();
        kv.setView(panel);
        kv.setBackground(Color.green);
        
        scrollPane = new KScrollPane();
        scrollPane.setVerticalScrollBarPolicy(KScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        scrollPane.setHorizontalScrollBarPolicy(KScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        scrollPane.setViewport(kv);
        
        getContentPane().add(scrollPane);
		
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
        Insets i = panel.getInsets();
        r.x -= i.left; r.y -= i.top; r.width += i.left+i.right; r.height += i.top+i.bottom;
        if (placeButtons || placeBackground)
            System.err.println("setting bounds "+r+", including insets "+panel.getInsets());
        panel.setTrueBounds(r);
    	// this seems to be the only way to tell a scroll pane what to put in its bars 
    	// (http://java.sun.com/docs/books/tutorial/uiswing/components/problems.html and elsewhere)
    	panel.setPreferredSize(new Dimension(r.width, r.height));
    	panel.revalidate();
        if (placeButtons || placeBackground) {
            Point p = new Point(0,0);
            SwingUtilities.convertPointToScreen(p, panel);
            System.err.println("after setTrueBounds and setPreferredSize, bounds are "+panel.getBounds()+
                               " and origin is at "+p);
        }
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
        NoneWindowSwing window = new NoneWindowSwing();
        Insets insets = window.getInsets();
        window.inAnApplet = false;

        window.setTitle("Absolute Positioning");
        window.setVisible(true);
    }
}
