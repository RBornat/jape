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
    
    private class KPanel extends JPanel {


        Point origin = new Point(0,0);
        Point viewPos = new Point(0,0);
        Dimension trueSize = new Dimension(0,0);

        public void setViewX(int x) {
            viewPos.x=x; validate();
        }

        public void setViewY(int y) {
            viewPos.y=y; validate();
        }

        public void setOrigin(int x, int y) {
            if (true)
                System.err.println("setting origin to "+x+","+y);
            origin.x=x; origin.y=y;
            //super.setLocation(x,y);
        }

        public void setTrueSize(int w, int h) {
            if (true)
                System.err.println("setting true size to "+w+"x"+h);
            trueSize.width=w; trueSize.height=h;
        }
        
        public void setTrueBounds(Rectangle r) {
            setOrigin(r.x,r.y);
            setTrueSize(r.width, r.height);
        }

        public void validate() {
            System.err.println("validating");
            Rectangle r = new Rectangle(0,0,0,0);
            r.add(b1.getBounds());
            r = b1.getBounds();
            r.add(b2.getBounds()); r.add(b3.getBounds());
            Insets i = panel.getInsets();
            r.x -= i.left; r.y -= i.top; r.width += i.left+i.right; r.height += i.top+i.bottom;
            if (true)
                System.err.println("setting bounds "+r+", including insets "+panel.getInsets());
            setTrueBounds(r);
            Rectangle vis = getBounds(); // seems to be what it does ...
            vis.translate(viewPos.x, viewPos.y);
            if (v!=null && vis.y<=r.y && r.y+r.height<=vis.y+vis.height)
                v.setValues(vis.y,0,vis.y,vis.y);
            else
                v.setValues(vis.y, vis.height, Math.min(vis.y,r.y), Math.max(vis.y+vis.height,r.y+r.height));
            if (h!=null && vis.x<=r.x && r.x+r.width<=vis.x+vis.width)
                h.setValues(vis.x,0,vis.x,vis.x);
            else
                h.setValues(vis.x, vis.width, Math.min(vis.x,r.x), Math.max(vis.x+vis.width,r.x+r.width));
            // this seems to be the only way to tell a scroll pane what to put in its bars
            // (http://java.sun.com/docs/books/tutorial/uiswing/components/problems.html and elsewhere)
            panel.setPreferredSize(new Dimension(r.width, r.height));
            panel.revalidate();
            /*if (v!=null) {
                v.setMinimum(
        }*/
            if (true) {
                Point p = new Point(0,0);
                SwingUtilities.convertPointToScreen(p, panel);
                System.err.println("after setTrueBounds and setPreferredSize, bounds are "+panel.getBounds()+
                                   " and origin is at "+p);
            }
        }

        Point lastPaintedOrigin = null;
        //Point lastPaintedViewPosition = null;
        Dimension lastPaintedSize = null;
        Shape lastPaintedClip = null;
        
        private boolean mustshow(Graphics g) {
            //Point viewPosition = scrollPane.getViewport().getViewPosition();
            Dimension size = getSize();
            Shape clip = g.getClip();
            boolean show = true && (lastPaintedOrigin==null ||
                                         !lastPaintedOrigin.equals(origin) ||
                                         //!lastPaintedViewPosition.equals(viewPosition) ||
                                         !lastPaintedSize.equals(size) ||
                                         !lastPaintedClip.equals(clip));
            if (show) {
                if (lastPaintedOrigin==null) {
                    lastPaintedOrigin = new Point(origin.x, origin.y);
                    //lastPaintedViewPosition=new Point(viewPosition.x,viewPosition.y);
                    lastPaintedSize = new Dimension(size.width, size.height);
                    lastPaintedClip = clip;
                }
                else {
                    lastPaintedOrigin.x=origin.x; lastPaintedOrigin.y=origin.y;
                    //lastPaintedViewPosition.x=viewPosition.x; lastPaintedViewPosition.y=viewPosition.y;
                    lastPaintedSize.width = size.width; lastPaintedSize.height = size.height;
                    lastPaintedClip = clip;
                }
            }
            return show;
        }

        public void paint(Graphics g) {
            if (false) {
                boolean show = mustshow(g);
                if (show) {
                    //Point viewPosition = scrollPane.getViewport().getViewPosition();
                    Point p = new Point(0,0);
                    SwingUtilities.convertPointToScreen(p, this);
                    System.err.print("paint("+g+")"+/*at "+viewPosition.x+","+viewPosition.y+*/", clip="+g.getClip()+
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
            if (true) {
                // System.err.println("translating children by "+viewPos.x+","+viewPos.y);
                g.translate(-viewPos.x, -viewPos.y);
                super.paintChildren(g);
                g.translate(viewPos.x,viewPos.y);
            }
            else
                super.paintChildren(g);
        }
    }

    private KPanel panel;
    private JScrollBar v, h;
    
    public NoneWindowSwing() {
        String yes = "Yes", no = "No";
        String[] buttons = { yes, no };

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
        
        /*KViewport kv = new KViewport();
        kv.setView(panel);
        kv.setBackground(Color.green);
        
        scrollPane = new KScrollPane();
        scrollPane.setVerticalScrollBarPolicy(KScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        scrollPane.setHorizontalScrollBarPolicy(KScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        scrollPane.setViewport(kv);
        
        getContentPane().add(scrollPane);*/
        getContentPane().setLayout(new BorderLayout());
        // System.err.println("layout is "+getContentPane().getLayout());
        v = new JScrollBar(JScrollBar.VERTICAL);
        class V implements AdjustmentListener {
            public void adjustmentValueChanged(AdjustmentEvent e) {
                switch (e.getAdjustmentType()) {
                    case AdjustmentEvent.TRACK:
                        System.err.println("V track "+v.getValue()); panel.setViewY(v.getValue()); panel.validate(); panel.repaint(); break;
                    default:
                        System.err.println("V sees AdjustmentEvent "+e.getAdjustmentType()); break;
                }
            }
        }
        v.addAdjustmentListener(new V());
        h = new JScrollBar(JScrollBar.HORIZONTAL);
        getContentPane().add(v, BorderLayout.EAST);
        getContentPane().add(h, BorderLayout.SOUTH);
        getContentPane().add(panel, BorderLayout.CENTER);
        getContentPane().setBackground(Color.black);
        panel.addComponentListener(new ComponentAdapter() {
            public void componentMoved(ComponentEvent e) {
                System.err.println("panel moved to "+panel.getLocation());
                panel.validate();
            }
            public void componentResized(ComponentEvent e) {
                System.err.println("panel resized to "+panel.getSize());
                panel.validate();
            }
        });	
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

        panel.validate();
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
