/*
 * Adapted from the Sun java tutorial example.
 *
 * The 'three' button moves SouthEast when clicked; the scroll pane copes fine.
 * 
 * The 'two' button moves West; it's drawn in the right position but when it 
 * crosses the western boundary (the y axis) the scroll pane doesn't indicate
 * properly.  Instead the _eastern_ extent shown as canvas (viewport?) background
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

import javax.swing.JOptionPane;

public class NoneWindowAWT extends Frame implements ActionListener {
    private Button b1, b2, b3;
    
    private class MyCanvas extends Canvas {
        public void setLocation(int x, int y) {
            if (placeBackground || placeButtons)
                System.err.println("canvas setLocation "+x+","+y);
            super.setLocation(x,y);
        }

        public void setLocation(Point p) {
            if (placeBackground || placeButtons)
                System.err.println("canvas setLocation "+p);
            super.setLocation(p);
        }

        public void setBounds(Rectangle r) {
            if (placeBackground || placeButtons)
                System.err.println("canvas setBounds "+r);
            super.setBounds(r);
        }

        public void setBounds(int x, int y, int w, int h) {
            if (placeBackground || placeButtons)
                System.err.println("canvas setBounds "+x+","+y+","+w+","+h);
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
        Point lastPaintedScrollPosition = null;
        Dimension lastPaintedSize = null;
        Shape lastPaintedClip = null;
        
        private boolean mustshow(Graphics g) {
            Point scrollPosition = scrollPane.getScrollPosition();
            Dimension size = getSize();
            Shape clip = g.getClip();
            boolean show = placeBackground || placeButtons && (lastPaintedOrigin==null ||
                                         !lastPaintedOrigin.equals(origin) ||
                                         !lastPaintedScrollPosition.equals(scrollPosition) ||
                                         !lastPaintedSize.equals(size) ||
                                         !lastPaintedClip.equals(clip));
            if (show) {
                if (lastPaintedOrigin==null) {
                    lastPaintedOrigin = new Point(origin.x, origin.y);
                    lastPaintedScrollPosition=new Point(scrollPosition.x,scrollPosition.y);
                    lastPaintedSize = new Dimension(size.width, size.height);
                    lastPaintedClip = clip;
                }
                else {
                    lastPaintedOrigin.x=origin.x; lastPaintedOrigin.y=origin.y;
                    lastPaintedScrollPosition.x=scrollPosition.x; lastPaintedScrollPosition.y=scrollPosition.y;
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
                    Point scrollPosition = scrollPane.getScrollPosition();
                    System.err.print("paint("+g+") at "+scrollPosition.x+","+scrollPosition.y+", clip="+g.getClip()+
                                     ", bounds="+getBounds());
                }
                Graphics g1 = g.create(origin.x, origin.y, getWidth(), getHeight());
                if (show) {
                    System.err.println(", translated to "+g1+", clip="+g1.getClip());
                }
                paintMyAll(g1);
                g1.dispose();
            }
            else
                paintMyAll(g);
        }

        protected void paintMyAll(Graphics g) {
            Rectangle r = getBounds();
            System.err.println("painting canvas: bounds are "+r+", origin is "+origin);
            g.setColor(getBackground());
            g.fillRect(r.x, r.y, r.width, r.height);
            if (placeButtons) {
                g.translate(-origin.x, -origin.y);
                paintMyChildren(g);
                g.translate(origin.x,origin.y);
            }
            else
                paintMyChildren(g);
        }

        protected void paintMyChildren(Graphics g) {
            b1.paint(g);
            b2.paint(g);
            b3.paint(g);
        }

        public void computeBounds() {
            Rectangle r = new Rectangle(0,0,0,0);
            r.add(b1.getBounds());
            // r = b1.getBounds();
            r.add(b2.getBounds()); r.add(b3.getBounds());
            if (placeButtons || placeBackground)
                System.err.println("setting bounds "+r);
            setTrueBounds(r);
            // this seems to be the only way to tell a scroll pane what to put in its bars
            // (http://java.sun.com/docs/books/tutorial/uiswing/components/problems.html and elsewhere)
            // but it isn't needed, or even possible, in AWT.
            // canvas.setPreferredSize(r.width, r.height);
            if (scrollPane!=null)
                scrollPane.doLayout();
            if (true || placeButtons || placeBackground) {
                System.err.println("after setTrueBounds and doLayout, bounds are "+canvas.getBounds()+" and preferredSize is "+canvas.getPreferredSize());
            }
        }
    }

    private class MyScrollPane extends ScrollPane {
        MyScrollPane() {
            super();
        }
        MyScrollPane(int policy) {
            super(policy);
        }
        // experiments in here so far unsuccessful.
    }
    
    private MyCanvas canvas;
    private MyScrollPane scrollPane;

    private final boolean placeBackground, placeButtons;

    public NoneWindowAWT() {
        String yes = "Yes", no = "No";
        String[] buttons = { yes, no };
        placeBackground = JOptionPane.showOptionDialog(null, "attempt to place canvas background correctly?", null, 0,
                                     JOptionPane.QUESTION_MESSAGE,null,buttons, no)==0;
        placeButtons = JOptionPane.showOptionDialog(null, "attempt to place buttons relative to background?", null, 0,
                                                    JOptionPane.QUESTION_MESSAGE,null,buttons, no)==0;

        canvas = new MyCanvas();
        
        b1 = new Button("one"); b1.addActionListener(this); 
        b2 = new Button("two"); b2.addActionListener(this); 
        b3 = new Button("three"); b3.addActionListener(this); 

        b1.setBounds(25, 5, 75, 20);
        b2.setBounds(55, 35, 75, 20);
        b3.setBounds(150, 15, 75, 30);

        //canvas.setBorder(new SoftBevelBorder(SoftBevelBorder.LOWERED, Color.red, Color.blue));
        canvas.setBackground(Color.yellow);
        canvas.computeBounds();
        
        scrollPane = new MyScrollPane(MyScrollPane.SCROLLBARS_ALWAYS);
        scrollPane.setBackground(Color.green);
        scrollPane.add(canvas);
        
        add(scrollPane);
		
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                System.exit(0);
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

        canvas.computeBounds();
    	canvas.repaint();
    }

    public static void main(String args[]) {
        try {
            // UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
        } catch (Exception e) {
            System.err.println("couldn't set L&F javax.swing.plaf.metal.MetalLookAndFeel");
        }
        NoneWindowAWT window = new NoneWindowAWT();
        Insets insets = window.getInsets();

        window.setTitle("Absolute Positioning");
        window.pack();
        window.setVisible(true);
    }
}
