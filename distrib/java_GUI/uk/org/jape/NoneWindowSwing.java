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

    // this is a panel so that it can have a background, a border and a position.
    // the clever bit (if it works) is the KComponent it contains
    private class KPanel extends JPanel {

        KPanel() { super(); setLayout(new KPanelLayout()); }
        KPanel(KComponent child) { this(); setChild(child); }

        protected KComponent child;

        protected void setChild(KComponent child) {
            this.child=child;
            super.add(child);
            revalidate();
        }

        private void ensureChild() {
            if (child==null) {
                setChild(new KComponent());
            }
        }
        
        public Component add(Component c) {
            ensureChild();
            child.add(c);
            child.validate();
            return c;
        }

        public final int ANCHOR_NORTH = 0;
        public final int ANCHOR_NORTHEAST = 1;
        public final int ANCHOR_EAST = 2;
        public final int ANCHOR_SOUTHEAST = 3;
        public final int ANCHOR_SOUTH = 4;
        public final int ANCHOR_SOUTHWEST = 5;
        public final int ANCHOR_WEST = 6;
        public final int ANCHOR_NORTHWEST = 7;

        // call this on a KPanel that already has a size, please.
        public void setChildAnchor(int anchor) {
            int x, y;
            switch (anchor) {
                case ANCHOR_NORTH:     x=getWidth()/2; y=0;             break;
                case ANCHOR_NORTHEAST: x=getWidth();   y=0;             break;
                case ANCHOR_EAST:      x=getWidth();   y=getHeight()/2; break;
                case ANCHOR_SOUTHEAST: x=getWidth();   y=getHeight();   break;
                case ANCHOR_SOUTH:     x=getWidth()/2; y=getHeight();   break;
                case ANCHOR_SOUTHWEST: x=0;            y=getHeight();   break;
                case ANCHOR_WEST:      x=0;            y=getHeight()/2; break;
                case ANCHOR_NORTHWEST: x=0;            y=0;             break;
                default: System.err.println("setChildAnchor "+anchor); return;
            }
            ensureChild();
            child.setLocation(x,y);
            revalidate();
        }

        public void computeBounds() {
            getLayout().layoutContainer(this);
        }
        
        public void paintChildren(Graphics g) {
            if (child!=null) {
                int x=child.getX(), y=child.getY();
                g.translate(x, y);
                child.paint(g);
                g.translate(-x,-y);
            }
        }
        
        protected class KPanelLayout implements LayoutManager {

            /* Called by the Container add methods. Layout managers that don't associate
            * strings with their components generally do nothing in this method.
            */
            public void addLayoutComponent(String s, Component c) { }

            /* Called by the Container remove and removeAll methods. Many layout managers
            * do nothing in this method, relying instead on querying the container for its
            * components, using the Container getComponents method.
            */
            public void removeLayoutComponent(Component c) { }

            /* Called by the Container getPreferredSize method, which is itself called under
            * a variety of circumstances. This method should calculate and return the ideal
            * size of the container, assuming that the components it contains will be at or
            * above their preferred sizes. This method must take into account the container's
            * internal borders, which are returned by the getInsets method.
            */
            public Dimension preferredLayoutSize(Container c) {
                layoutContainer(c);
                return getSize();
            }

            /* Called by the Container getMinimumSize method, which is itself called under
            * a variety of circumstances. This method should calculate and return the minimum
            * size of the container, assuming that the components it contains will be at or
            * above their minimum sizes. This method must take into account the container's
            * internal borders, which are returned by the getInsets method.
            */
            public Dimension minimumLayoutSize(Container c) { return preferredLayoutSize(c); }

            /* Called when the container is first displayed, and each time its size changes.
            * A layout manager's layoutContainer method doesn't actually draw components.
            * It simply invokes each component's resize, move, and reshape methods to set
            * the component's size and position. This method must take into account the
            * container's internal borders, which are returned by the getInsets method.
            * You can't assume that the preferredLayoutSize or minimumLayoutSize method
            * will be called before layoutContainer is called.
            */
            public void layoutContainer(Container c) {
                if (child!=null) {
                    child.getLayout().layoutContainer(child);
                    Rectangle vr = child.getVisualBounds();
                    Insets i = c.getInsets();
                    vr.x -= i.left; vr.width += i.left+i.right;
                    vr.y -= i.top; vr.height += i.top+i.bottom;
                    // new top is at cx+vcx; new left at cy+vcy.
                    // we must adjust child location when we adjust our own
                    int deltax = child.getX()+vr.x, deltay=child.getY()+vr.y;
                    if (deltax!=0 || deltay!=0) {
                        setLocation(getX()+deltax, getY()+deltay);
                        child.setLocation(-vr.x, -vr.y);
                        repaint();
                    }
                    Dimension size = getSize();
                    if (vr.width!=size.width || vr.height!=size.height) {
                        size.width=vr.width; size.height=vr.height;
                        setSize(size); setPreferredSize(size); setMinimumSize(size);
                    }
                }
            }
        }
    }

    public class KComponent extends JComponent {
        KComponent() { super(); setLayout(new KComponentLayout()); }

        protected Rectangle visualBounds = new Rectangle(0,0,0,0);

        public Rectangle getVisualBounds() {
            return new Rectangle(visualBounds.x, visualBounds.y, visualBounds.width, visualBounds.height);
        }

        public boolean contains(int x, int y) {
            return visualBounds.contains(x,y);
        }

        protected class KComponentLayout implements LayoutManager {
            /* Called by the Container add methods. Layout managers that don't associate
            * strings with their components generally do nothing in this method.
            */
            public void addLayoutComponent(String s, Component c) { }

            /* Called by the Container remove and removeAll methods. Many layout managers
            * do nothing in this method, relying instead on querying the container for its
            * components, using the Container getComponents method.
            */
            public void removeLayoutComponent(Component c) { }

            /* Called by the Container getPreferredSize method, which is itself called under
            * a variety of circumstances. This method should calculate and return the ideal
            * size of the container, assuming that the components it contains will be at or
            * above their preferred sizes. This method must take into account the container's
            * internal borders, which are returned by the getInsets method.
            */
            public Dimension preferredLayoutSize(Container c) {
                layoutContainer(c);
                return getSize();
            }

            /* Called by the Container getMinimumSize method, which is itself called under
            * a variety of circumstances. This method should calculate and return the minimum
            * size of the container, assuming that the components it contains will be at or
            * above their minimum sizes. This method must take into account the container's
            * internal borders, which are returned by the getInsets method.
            */
            public Dimension minimumLayoutSize(Container c) { return preferredLayoutSize(c); }

            /* Called when the container is first displayed, and each time its size changes.
            * A layout manager's layoutContainer method doesn't actually draw components.
            * It simply invokes each component's resize, move, and reshape methods to set
            * the component's size and position. This method must take into account the
            * container's internal borders, which are returned by the getInsets method.
            * You can't assume that the preferredLayoutSize or minimumLayoutSize method
            * will be called before layoutContainer is called.
            */
            public void layoutContainer(Container c) {
                Rectangle newBounds=null;
                int nc = getComponentCount();
                for (int i=0; i<nc; i++) {
                    if (newBounds==null)
                        newBounds = getComponent(i).getBounds();
                    else
                        newBounds.add(getComponent(i).getBounds());
                }
                if (newBounds==null)
                    newBounds = new Rectangle(0,0,0,0);
                Dimension size = getSize();
                if (newBounds.width!=size.width || newBounds.height!=size.height) {
                    size.width=newBounds.width; size.height=newBounds.height;
                    setSize(size); setPreferredSize(size); setMinimumSize(size);
                }
                if (!newBounds.equals(visualBounds)) {
                    visualBounds=newBounds;
                    repaint();
                }
            }
        }
    }
    
    private KPanel panel;
    private JScrollPane scrollPane;
    
    public NoneWindowSwing() {
        String yes = "Yes", no = "No";
        String[] buttons = { yes, no };

        panel = new KPanel();
        
        bN = new JButton("North"); bN.setBounds(25, 5, 75, 20); bN.addActionListener(this); panel.add(bN);
        bE = new JButton("East"); bE.setBounds(55, 35, 75, 20); bE.addActionListener(this); panel.add(bE);
        bS = new JButton("South"); bS.setBounds(150, 15, 75, 30); bS.addActionListener(this); panel.add(bS);
        bW = new JButton("West"); bW.setBounds(150, -5, 75, 30); bW.addActionListener(this); panel.add(bW);

        panel.setBorder(new SoftBevelBorder(SoftBevelBorder.LOWERED, Color.red, Color.blue));
        panel.setBackground(Color.yellow);
        panel.setLocation(100,100);

        panel.computeBounds();
        System.err.println("panel with four buttons has size "+panel.getPreferredSize());

        Container contentPane = getContentPane();
        Rectangle panelBounds = panel.getBounds();

        scrollPane = new JScrollPane(panel);
        
        contentPane.add(scrollPane);
        contentPane.doLayout();

        // nothing seems to give us a properly-sized window ...
        //setSize(Math.min(500, Math.max(200,panelBounds.x+panelBounds.width+50)),
        //        Math.min(400, Math.max(200, panelBounds.y+panelBounds.height+50)));

        System.err.println("window bounds are "+getBounds());
        
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
            Rectangle r = bN.getBounds();
            bN.setBounds(r.x, r.y-50, r.width, r.height);
            System.err.println("North at "+bN.getBounds());
        }
        else
        if (s.equals("East")) {
            Rectangle r = bE.getBounds();
            bE.setBounds(r.x+50, r.y, r.width, r.height);
            System.err.println("East at "+bE.getBounds());
        }
        else
        if (s.equals("South")) {
            Rectangle r = bS.getBounds();
            bS.setBounds(r.x, r.y+50, r.width, r.height);
            System.err.println("South at "+bS.getBounds());
        }
        else
        if (s.equals("West")) {
                Rectangle r = bW.getBounds();
                bW.setBounds(r.x-50, r.y, r.width, r.height);
                System.err.println("West at "+bW.getBounds());
        }

        panel.revalidate();
    	panel.repaint();
    }

    public static void main(String args[]) {
        try {
            // UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
        } catch (Exception e) {
            System.err.println("couldn't set L&F javax.swing.plaf.metal.MetalLookAndFeel");
        }
        NoneWindowSwing window = new NoneWindowSwing();
        window.inAnApplet = false; /* really? */

        window.setTitle("Absolute Positioning with negative coordinates in a panel");
        // window.pack(); // gives a zero-dimensioned pane, I kid you not.
        window.setVisible(true);
    }
}
